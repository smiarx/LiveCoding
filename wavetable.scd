(
var signals, buffers, bufPerOctave=2, midiLo = 0, midiHi = 98, size=4096;
var bufPerTable;
var imag, cosTable;

signals = [
{|x| sin(2pi*x)},
{|x| 2*abs(2*(x+0.25 - floor(x+0.75))) - 1},
{|x| 2*(x - floor(x+0.5))},
{|x| ((x - floor(x+0.5)) >= 0).asInteger*2 - 1}
];

bufPerTable =
	((midiHi-midiLo) / (12/bufPerOctave)).asInt;
buffers = Buffer.allocConsecutive(
	bufPerTable * signals.size,
	s,
	size * 2,
);


imag = Signal.newClear(size);
cosTable = Signal.fftCosTable(size);

signals.do{|func, j|
	var real, complex;

	real = Signal.newClear(size);
	real.waveFill(func, 0, 1);
	real = real - mean(real);

	complex = fft(real, imag, cosTable);

	(bufPerTable).do{ |i|
		var ifft, fcomplex, ctf;
		fcomplex = complex.copy;
		ctf = (s.sampleRate/2 / (midiLo+(i*12/bufPerOctave)).midicps).floor.asInteger;
		fcomplex.real[ctf..(size-ctf)] = 0;
		fcomplex.imag[ctf..(size-ctf)] = 0;

		ifft = fcomplex.real.ifft(fcomplex.imag, cosTable);
		buffers[(i*signals.size)+j].sendCollection(ifft.real.asWavetable);
	}
};
b = buffers;

);

(
SynthDef(\wavetable, {
	var out      = \out.ir(0);
	var pan      = \pan.ir(0);
	var midinote = \midinote.ir(39);
	var cutoff   = \cutoff.ir(36);
	var resonance= \resonance.ir(2);
	var velocity = \velocity.ir(1);
	var pwm      = \pwm.ir(0.5);
	var sub      = \sub.ir(0.4);
	var lfo      = \lfo.ir(0);
	var lfocutoff= \lfocutoff.ir(5);
	var vibrato  = \vibrato.ir(0.1);
	var attack   = \attack.ir(0.001);
	var susgain  = \sustaingain.ir(0.5);
	var release  = \release.ir(0.4);
	var decay    = \decay.ir(0.098);
	var sustain  = \sustain.ir(1);
	var buf= \buf.ir(b[0].bufnum);
	var sig, env, freq;

	lfo = In.kr(~lfobus.index + lfo, 1);
	freq = (midinote + (lfo*vibrato)).midicps;

	env = Env([0,1,susgain,susgain,0], [attack,decay,sustain-(attack+decay),release], -2).ar(
		Done.freeSelf,
		//levelScale: 0.4,)
		timeScale: AmpComp.ir(freq, 150, 0.13)
	);

	sig = VOsc.ar(buf+((midinote/6).floor*4) +
		max(0,min(4,((env*0.4+ (lfo*0.4)+ 0.4)*(4)))), freq, SinOsc.ar(freq/2,0,velocity*env+2));
	sig = MoogFF.ar(sig, freq*(cutoff+(env*velocity)+(lfo*(lfocutoff))).midiratio, resonance);
	sig = sig * env;

	Out.ar(out, DirtPan.ar(sig, 2, pan));
}).add
)


MIDIClient.init;
MIDIIn.connectAll;
(
var notes = Array.newClear(128);

MIDIdef.noteOn(\noteOn, {
|vel, num|
notes[num].release;
	notes[num] = Synth(\wave, [\freq, num.midicps, \buf, b[((((num/6).floor)).asInt*4).postln], \vel, (vel/127)]);
});

MIDIdef.noteOff(\noteOff, {
|vel, num|
notes[num].release;
notes[num] = nil;
});
)


b[4].plot;

(
{
	var freq=70*([-1,0,1]*0.1).midiratio;
	var sig, lfo;

	lfo = LFTri.kr(LFNoise0.kr([0.1,0.12]).range(0.3, 12));
	sig = VOsc.ar(
		(b[0].bufnum + ((freq).cpsmidi / 6).floor * 6) +
		LFNoise2.kr(1.2).range(0,10)
		,
		freq,
		SinOsc.ar(freq/4 *
			LFNoise0.kr(LFNoise0.kr(0.3).range(0.02,0.8)).range(1,6).floor
		) * LFNoise1.kr(0.8).range(0,10)
	);
	sig = tanh(Splay.ar(sig, 1, 1.2, LFNoise1.kr(0.2))) * 0.5;
	sig = MoogFF.ar(sig, LFNoise2.kr(0.7).exprange(700,9000) + (lfo*30).midicps, 0.1);
}.play
)

(
{
	var freq=120*([0]*0.1).midiratio;
	var sig, lfo;

	//lfo = LFTri.kr(LFNoise0.kr([0.1,0.12]).range(0.3, 12));
	sig = VOsc.ar(b[0].bufnum + (freq.cpsmidi /6).floor * 3, freq);
	Out.ar(0, (sig*0.2).dup);
}.play
)