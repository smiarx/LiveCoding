////// Controls //////
//a MIDIClient.init;
// MIDIIn.connectAll;

MIDIFunc.trace(false);

~ccnum =
(1: "1", 2:"2", 3:"3", 4:"4");

~osc = NetAddr.new("127.0.0.1", 6010);

~ccnum.keysValuesDo{ |cc, name|
	MIDIFunc.cc({
		|val|
		~osc.sendMsg("/ctrl", name.asString, val/127);
	}, cc);
};


// Voice

~voicenum = (
	voice1: 48,
	voice2: 49,
	voice3: 50,
	voice4: 51
);
~voice_record = 1;
~voicestart = ();
~recorder = ();
~voicebuffer = Buffer.alloc(s, s.sampleRate * 10, 1);
~bufs = ();


SynthDef(\voice_record, {
	| in, buf |
	var sig;
	sig = SoundIn.ar(in);
	RecordBuf.ar(sig, buf, doneAction:Done.freeSelf, loop:0);
}).add;


~voicenum.keysValuesDo { |name, cc|
	MIDIFunc.noteOn({
		name.postln;
		~recorder[name] = Synth(\voice_record,
			[\in, ~voice_record, \buf, ~voicebuffer]);
		~voicestart[name] = TempoClock.seconds;
	}, cc);

	MIDIFunc.noteOff({
		var time, fadesize = 100, hanning, fadein, fadeout;
		(name ++ "off").postln;
		~recorder[name].free;
		~recorder[name] = nil;
		time = min(TempoClock.seconds - ~voicestart[name], 10);
		~bufs[name].notNil.if{
			~bufs[name].free
		};
		time.postln;
		~bufs[name] = Buffer.alloc(s, time*s.sampleRate);
		{
			0.2.wait;
			~voicebuffer.copyData(~bufs[name]);
			~dirt.soundLibrary.addBuffer(name, ~bufs[name], false);
		}.fork


	}, cc);
};


SynthDef(\in, { |out, sustain = 1, speed = 1, begin = 0, end = 1, pan, accelerate, offset|
	var env, sound, rate, phase;
	phase = Line.ar(begin, end, sustain);
	rate = (begin + 1) * (speed + Sweep.kr(1, accelerate));
	sound = SoundIn.ar([1]).dup; // stereo in
	OffsetOut.ar(out,
		DirtPan.ar(sound, ~dirt.numChannels, pan)
	)
}).add
