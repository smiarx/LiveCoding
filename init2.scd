(
var numChannels = 2;
var numOrbits = 4;
var ardour = NetAddr("localhost", 3819);

var numlfo=3, lfo;
var phaserlfo, phaserbus;


"SC_JACK_DEFAULT_OUTPUTS".setenv(
	numOrbits.collect{ |o|
		numChannels.collect{ |c|
			"ardour:Orbit %/audio_in %".format(o, c+1);
		}
	}.flatten.join(",").postln
);

s.options.numBuffers = 1024 * 256;
s.options.memSize = 8192 * 16;
s.options.maxNodes = 1024 * 32;
s.options.numOutputBusChannels = numChannels*numOrbits;
s.options.numInputBusChannels = 2;

s.waitForBoot {
	~dirt = SuperDirt(numChannels, s);
	~dirt.loadSoundFiles;
	~dirt.loadSoundFiles("/home/sirbu/projects/supercollider/LiveCoding/samples/*");

	SynthDef("dirt_lfo", {
		var out = \out.ir;
		var rate = \rate.kr(0.5);
		Out.kr(out, LFPar.kr(rate));
	}).add;

	~lfobus = Bus.control(~dirt.server, numlfo);
	phaserbus = Bus.control(s,1);

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

	SynthDef("dirt_phaser" ++ numChannels, {
		var out = \out.ir(0);
		var depth = \phasedepth.ir(0.1);
		var feedback = \phaserfeedback.ir(0.2);
		var lfo = \lfo.kr(0);
		var in,mixed,mod, phased;
		var center = 0.85;

		depth = min(min(depth, 1-center-0.02), center-0.2);
		in = In.ar(out,numChannels);

		//mod = SinOsc.kr(phaserrate,[0,phase.mod(2pi)],phaserdepth,center);
		mod = (lfo*[1,-1]) * depth + center;
		phased = Phaser1.ar(in, mod!6, feedback);
		mixed = in+ phased;
		ReplaceOut.ar(out, mixed);
	}, [\ir, \ir, \ir]).add;

	s.sync;

	// start LFO
	lfo = numlfo.collect{ |i|
		Synth.before(~dirt.server.defaultGroup, \dirt_lfo, [\out, ~lfobus.index + i]);
	};
	phaserlfo = Synth.before(s.defaultGroup, \dirt_lfo, [\out, phaserbus]);

	s.sync;


	~dirt.start(57120, (0..numOrbits)*numChannels);

	~dirt.orbits.do{ |orbit|
		//orbit.globalEffects.do(_.release);
		orbit.globalEffects = [
			GlobalDirtEffect(\dirt_delay, [\delaytime, \delayfeedback, \delaySend, \delayAmp, \lock, \cps]),
			//GlobalDirtEffect(\dirt_sidechain_reverb, [\size, \room]),
			GlobalDirtEffect(\dirt_monitor).alwaysRun_(true);
		];
		orbit.initNodeTree;
	};

	~dirt.addModule('lfo',
		{ |dirtEvent|
			lfo[(~lfo ? 0)].set(\rate, ~lforate);
	}, {~lforate.notNil });

	~dirt.addModule('phaser',
	{ |dirtEvent|
			phaserlfo.set(\rate, ~phaserrate);
			dirtEvent.sendSynth('dirt_phaser' ++ ~numChannels,
				[
					phaserdepth: ~phaserdepth,
					phaserfeedback: ~phaserfeedback,
					lfo: phaserbus.asMap,
					out: ~out
			])
	}, { ~phaserrate.notNil });

	~dirt.addModule('lpf',
		{ |dirtEvent|
			dirtEvent.sendSynth("dirt_lpf" ++ ~numChannels,
				[
					cutoff: ~lpf,
					resonance: ~resonance,
					out: ~out
			])

	}, { ~lpf.notNil });

	// enveloppe only for samples
	~dirt.addModule('envelope',
		{ |dirtEvent|
			if(~instrument.asString.beginsWith("dirt_sample")){
				dirtEvent.sendSynth('dirt_envelope' ++ ~dirt.numChannels,
					[
						attack: ~attack,
						hold: ~hold,
						release:~release,
						out: ~out
				])
			}
	}, { ~attack.notNil or: { ~release.notNil }});

	~dirt.addModule('reverb',
		{ |dirtEvent|
			ardour.sendMsg("/strip/send/gain", dirtEvent.orbit.orbitIndex+1, 2, (~room ? (-inf)).ampdb);

	});

	~dirt.addModule('sidechain',
		{ |dirtEvent|
			ardour.sendMsg("/strip/send/gain", dirtEvent.orbit.orbitIndex+1, 1, (~sidechain ? (-inf)).ampdb);

	});

};

s.latency = 0.1;

)
