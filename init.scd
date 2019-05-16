// configure the sound server: here you could add hardware specific options
// see http://doc.sccode.org/Classes/ServerOptions.html
(
var numChannels;
s.options.numBuffers = 1024 * 256; // increase this if you need to load more samples
s.options.memSize = 8192 * 16; // increase this if you get "alloc failed" messages
s.options.maxNodes = 1024 * 32; // increase this if you are getting drop outs and the message "too many nodes"
s.options.numOutputBusChannels = 2; // set this to your hardware output channel size, if necessary
s.options.numInputBusChannels = 2; // set this to your hardware output channel size, if necessary
// boot the server and start SuperDirt
s.waitForBoot {
	~dirt = SuperDirt(2, s); // two output channels, increase if you want to pan across more channels
	~dirt.loadSoundFiles;   // load samples (path containing a wildcard can be passed in)
	~dirt.loadSoundFiles("/home/sirbu/projects/supercollider/LiveCoding/samples/*");
	// s.sync; // optionally: wait for samples to be read

	~busses = 4.collect { |i|
		Bus.audio(~dirt.server, ~dirt.numChannels);
	};

	~reverbbus = Bus.audio(~dirt.server, ~dirt.numChannels);

	~dirt.start(57120, ~busses);   // start listening on port 57120, create two busses each sending audio to channel 0


	///////////  EFFECTS ////////////
	numChannels = ~dirt.numChannels;
	SynthDef(\dirt_sidechain_reverb ++ numChannels, { |dryBus, effectBus, gate = 1, room|
		var in = In.ar(dryBus, numChannels);
		in = in * room.lag(LFNoise1.kr(1).range(0.01, 0.02));
		in = in * EnvGen.kr(Env.asr, gate, doneAction:2);

		DirtPause.ar(in, graceTime:4);

		Out.ar(~reverbbus, in);
	}, [\ir, \ir]).add;

	SynthDef("dirt_delay" ++ numChannels, { |dryBus, effectBus, gate = 1, delaytime, delayfeedback, delaySend = 1, delayAmp = 1, lock = 0, cps = 1|
		var signal;
		var input = In.ar(dryBus, numChannels);
		var maxDelayTime = 2;

		//input = input * delaySend.lag(LFNoise1.kr(1).range(0.01, 0.02)); // regulate input
		input = input * delayAmp.lag(LFNoise1.kr(1).range(0.01, 0.02)); // regulate input

		delayfeedback = delayfeedback.max(0);
		delaytime = delaytime * if(lock, reciprocal(cps), 1);
			delaytime = delaytime.clip(0, maxDelayTime); // just to be sure
		// from sc3-plugins
		signal = FaustTapedelay.ar(input, delaytime, delayfeedback, 120, 3200);

		//DetectSilence.ar(signal, time:1, doneAction: 1);
		// doesn't work properly. We don't really know when it's not used anymore.
		// this would have to be an explicit signal from sclang

		signal = signal * EnvGen.kr(Env.asr, gate, doneAction:2);
		//signal = signal * delayAmp.lag(0.01);

		DirtPause.ar(signal, graceTime:4);

		Out.ar(effectBus, signal);

	}, [\ir, \ir]).add;


	s.sync;

	~master = Synth.after(~dirt.server.defaultGroup, \dirt_master ++ ~dirt.numChannels ++ 4,
		[\busses, ~busses, \reverbbus, ~reverbbus, \sidechainbus, ~dirt.orbits[3].outBus]
	);

	~dirt.orbits.do{ |each|
		each.globalEffects = [
			GlobalDirtEffect(\dirt_delay, [\delaytime, \delayfeedback, \delaySend, \delayAmp, \lock, \cps]),
			GlobalDirtEffect(\dirt_sidechain_reverb, [\size, \room]),
			GlobalDirtEffect(\dirt_monitor).alwaysRun_(true);
		];
		each.freeSynths;
		each.initNodeTree;
	};

};
)

s.latency = 0.1; // increase this if you get "late" messages


(
~master.set(\reverb, 1.0, \size, 0.7, \distortion, 5);
)

~reverbbus
