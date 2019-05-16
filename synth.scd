
var algorithms =
[
	[40, [16,16, 4, 2, 1, 0]],
	[40, [16, 0, 4, 2, 1, 1]],
	[36, [16, 8, 0, 2, 1, 1]],
	[36, [16, 8, 0, 2, 1, 4]],
	[42, [16, 0, 4, 0, 1, 1]],
	[42, [16, 0, 4, 0, 1, 2]],
	[40, [16, 0, 6, 0, 1, 1]],
	[40, [16, 0, 6, 4, 1, 0]],
	[40, [16,16, 6, 0, 1, 0]],
	[36, [16, 8, 8, 3, 0, 0]],
	[36, [16, 8, 0, 3, 0, 1]],
	[40, [16,16, 7, 0, 0, 0]],
	[40, [16, 0, 7, 0, 0, 1]],
	[40, [16, 0, 4, 3, 0, 1]],
	[40, [16,16, 4, 3, 0, 0]],
	[32, [26, 0, 4, 0, 1, 1]],
	[32, [26,16, 4, 0, 1, 0]],
	[32, [28, 0, 8, 2, 1, 0]],
	[38, [16, 8, 0, 1, 1, 1]],
	[52, [ 8, 8, 8, 3, 0, 0]],
	[54, [ 8, 8, 8, 1, 1, 0]],
	[46, [16, 0, 1, 1, 1, 1]],
	[54, [ 0, 8, 0, 1, 1, 1]],
	[62, [ 0, 0, 1, 1, 1, 1]],
	[62, [ 0, 0, 0, 1, 1, 1]],
	[52, [ 0, 8, 0, 3, 0, 1]],
	[52, [ 0, 8, 8, 3, 0, 0]],
	[41, [16, 0, 4, 2, 2, 0]],
	[58, [ 0, 0, 4, 0, 1, 1]],
	[57, [ 0, 0, 4, 2, 2, 0]],
	[62, [ 0, 0, 0, 0, 1, 1]],
	[63, [ 0, 0, 0, 0, 0, 1]]
];

(
pia: (
	algorithm: 4,
	oplevel: [ -26.359866254608, -29.469113440704, -24.462880257483, -27.813406034375, -20.949666051237, -25.592712920381 ],
	levels: [
		[ 0.75614366729679, 0.9409, 0.79442344045369, 0.8836, 0.79442344045369, 0.79012345679012 ],
		[ 0.48393194706994, 0.42533081285444, 0.42533081285444, 0.51465028355388, 0.42533081285444, 0.45415879017013 ],
		[ 0.1531190926276, 0.047258979206049, 0.0382797731569, 0.047258979206049, 0.011814744801512, 0.079867674858223 ],
		[ 0, 0, 0, 0, 0, 0 ]
	],
	rates: [
		[ -10, -10, -10, -10, -10, -10 ],
		[ -0.80777927709566, -0.058304590720708, 0.80652307801469, 0.80652307801469, 0.32176249917214, 0.6553526035397 ],
		[ 3.6084706005514, 3.274368137773, 3.274368137773, 2.7139797386931, 3.3930724412685, 3.1476642277301 ],
		[ 0.16019740326461, -0.14863289986952, -0.14863289986952, -2.5533434518405, 0.15367884779234, -0.54305345235285 ]
	],
	pitchlevels:[0,0,0,0],
	pitchrates:[100,100,100,100],
	amd: 0,
	opamd: [ 0, 0, 0, 0, 0.28883898881706, 0 ],
	pmd: 0.34166666666667,
	opmode: [ 0, 0, 0, 0, 0, 0 ],
	opratio: [ 0, 0, 0, 0, 0, 0 ],
	opfixed: [ 69, 69, 69, 69, 69, 69 ],
	opdetune: [ 0.063115596771245, 0.063115596771245, -0.0090165138244635, 0, 0.0090165138244635, 0.0090165138244635 ],
	breakpoint: [ 69, 57, 57, 57, 57, 57 ],
	krs: [ 0.1, 0.1, 0.1, 0.1, 0.13333333333333, 0.13333333333333 ],
	lkls: [ 0, 0.21100917431193, 0, 0.10091743119266, 0.018181818181818, 0 ],
	rkls: [ -0.27272727272727, -0.13761467889908, -0.08695652173913, -0.30434782608696, 0.17391304347826, -0.29411764705882 ],
	lcurve: [ 0, 1, 1, 0, 0, 0 ],
	rcurve: [ 0, 1, 0, 0, 0, 0 ],
	lfo_delay: 0.4,
	opsensitivity: [ 0.71739130434783, 0.98165137614679, 0.69565217391304, 0.60869565217391, 0.58181818181818, 0.47826086956522 ],
	feedback: 0,
	cutoff: {\cutoff.ir(36)},
	res: {\resonance.ir(0.8)}
),
pof: (
	algorithm: 0,
	oplevel: [ -18.657884692262, -32.79746285393, -29.645926969927, -28.921062333608, -33.650533085258, -90.683784107619 ],
	levels: [
		[ 1, 1, 1, 1, 1, 1 ],
		[ 0.45008812690274, 0.27629848783695, 0.45008812690274, 0.92549270950168, 0.92549270950168, 0.92549270950168 ],
		[ 0, 0.16831032215648, 0, 0.87742348982535, 0.87742348982535, 0.87742348982535 ],
		[ 0, 0.065746219592373, 0, 0.53402366863905, 0.61592693478609, 0.34779750164366 ]
	],
	rates: [
		[ -10, -10, -10, -10, -10, -10 ],
		[ -2.828767920226, 2.262233998378, -2.828767920226, -4.6187861154354, -4.6187861154354, -4.6187861154354 ],
		[ 2.1939156609127, 3.3270640750476, 2.1939156609127, -1.1728403212181, -1.1728403212181, -1.1728403212181 ],
		[ -0.078614590576469, 2.571200698265, -0.078614590576469, 0.37444694450395, 0.11638524346573, 0.11638524346573 ]
	],
	pitchlevels: [0,0,0,0],
	pitchrates: [100,100,100,100],
	amd: 0,
	opamd: [ 0, 0, 1.3475467138972, 4.846846315149, 0, 0 ],
	pmd: 0.11046511627907,
	opmode: [ 0, 0, 0, 0, 0, 0 ],
	opratio: [ 0, -12, 12, -12, -12, -12 ],
	opfixed: [ 69, 69, 69, 69, 69, 69 ],
	opdetune: [ 0, 0, 0, 0, 0, 0 ],
	breakpoint: [ 69, 65, 69, 69, 69, 69 ],
	krs: [ 0.066666666666667, 0.066666666666667, 0.1, 0.2, 0.16666666666667, 0.1 ],
	lkls: [ -0.11392405063291, 0.025641025641026, -0.26582278481013, -0.076923076923077, 0, 0 ],
	rkls: [ 0, -0.17948717948718, -0.18987341772152, -0.41025641025641, -0.39240506329114, -0.051282051282051 ],
	lcurve: [ 0, 0, 0, 0, 0, 0 ],
	rcurve: [ 0, 0, 0, 0, 0, 0 ],
	lfo_delay: 0,
	opsensitivity: [ 0.48101265822785, 0.48717948717949, 1, 0, 0, 0.47435897435897 ],
	feedback: 0.11184210526316,
	lfo_rate: 1.1627906976744,
	lfo_type: 0,
	cutoff: {\cutoff.ir(28)},
	res: {\resonance.ir(0.3)}
),

bass: (
	algorithm: 5,
	oplevel: [ -19.997549464332, -26.193627863762, -18.876400520322, -27.041199826559, -23.164799306237, -40.912699275545 ],
	levels: [
		[ 1, 1.0, 1.0, 1.0, 1.0, 0.36 ],
		[ 0.64, 0.225625, 0.49, 0.225625, 0.49, 0.455625 ],
		[ 0.075625, 0.0, 0.455625, 0.0, 0.455625, 0.0 ],
		[ 0, 0.0, 0.0, 0.0, 0.0, 0.0 ],
	],
	rates: [
		[ -10, -9.057073694012, -10.0, -9.057073694012, -10.0, -9.057073694012 ],
		[ -0.33330271330492, -5.2711536853409, -0.33330271330492, -5.2711536853409, -0.33330271330492, 1.4516861720376 ],
		[ -1.0068610779088, 2.0174024802813, 1.7730724571885, 2.0174024802813, 1.7730724571885, 2.0174024802813 ],
		[ -0.14863289986952, -0.14863289986952, -0.56166274933147, -0.14863289986952, -0.56166274933147, -0.14863289986952 ],
	],
	pitchlevels: [0,0,0,0],
	pitchrates: [100,100,100,100],
	amd: 0,
	opamd: [ 0, 0, 0, 3.2548461839283, 0, 0.34829378960737 ],
	pmd: 0.13114754098361,
	opmode: [ 0, 0, 0, 0, 0, 0 ],
	opratio: [ 0.0, 12.0, 0.0, 12.0, 12.0, 12.0 ],
	opfixed: [ 69, 69, 69, 69, 69, 69 ],
	opdetune: [ 0.063115596771245, 0.063115596771245, -0.063115596771245, -0.063115596771245, 0.0090165138244635, 0.0090165138244635 ],
	breakpoint: [ 50.0, 62.0, 50.0, 50.0, 57.0, 57.0 ],
	krs: [ 0.1, 0.13333333333333, 0.13333333333333, 0.13333333333333, 0.13333333333333, 0.16666666666667 ],
	lkls: [ -0.05, 0.0, 0.05, -0.05, 0.0, 0.0 ],
	rkls: [ -0.35, -0.65, -0.5, -0.45, -0.55, -0.7 ],
	lcurve: [ 0, 0, 0, 0, 0, 0 ],
	rcurve: [ 0, 0, 0, 0, 0, 0 ],
	opsensitivity: [ 1.0, 1.0, 0.6, 0.75, 0.675, 0.85 ],
	lfo_delay: 0.0,
	lfo_rate: 2.1311475409836,
	lfo_type: 4,
	feedback: 0.1304347826087,
	cutoff: {\cutoff.ir(78)},
	res: {\resonance.ir(0.9)}
)
).keysValuesDo { |name, params|

	params.use{
		SynthDef("dx7."++name, {

			var
			midinote = \midinote.ir(69),
			sustain = \sustain.ir(1),
			pan = \pan.ir(0.5),
			lfo = \lfo.ir(0),
			gain = \amp.ir(0.8),
			velocity = \velocity.ir(1),
			//explutbuf = \explutbuf.ir,
			level = \level.kr(1),
			cutoff = ~cutoff.value(),
			res = ~res.value(),
			out = \out.ir(0);

			var explutbuf = #[0,0.2493501787509,0.49870035750181,0.74805053625271,0.99740071500361,1.2467508937545,1.4961010725054,1.7454512512563,1.9948014300072,2.2441516087581,2.7428519662599,3.4909025025127,3.9896028600145,4.7376533962672,5.7350541112708,6.7324548262744,8.2285558987798,9.7246569712852,11.719458401292,13.963610010051,16.45711179756,19.948014300072,23.438916802585,27.428519662599,31.418122522614,35.407725382628,39.397328242643,43.386931102657,47.376533962672,51.366136822686,55.355739682701,59.345342542715,62.337544687726].as(LocalBuf);

			var createEG;
			var op, audio, env, oppitch, opamp;
			var kls, offset, klsLin,klsExp, klsll, klsle, klsrl, klsre;
			var pitch, pitchEG;

			lfo = In.kr(lfo);

			if(~lfo_delay.isUGen || ~lfo_delay != 0){
				lfo = lfo * XLine.kr(0.00001,1,~lfo_delay);
			};

			//pitch

			//pitchEG = EnvGen.ar([~pitchlevels[3]]+~pitchlevels, 2**(~pitchrates), \sin);
			pitch = midinote + (~pmd*lfo);// + pitchEG;



			//EG
			~krs = (midinote/3 - 7).max(0).min(31) * ~krs;

			~times = 2**(~rates-[~krs]);
			~sustain = sustain-~times[0]-~times[1]-~times[2]-~times[3];
			env = Env(
				~levels[[3,0,1,2,2,3]],
				~times[0..2] ++ ~sustain.postln ++ [~times[3]],
				[-2,-2,-3,3]).ar();


			oppitch = (((1-~opmode)*(pitch+~opratio)) + (~opmode*~opfixed))+~opdetune;




			//lKeyboard level scaling (max 2db/ semi-tone)
			offset = (~breakpoint - midinote).abs;

			klsLin = offset*2;
			klsll = klsLin*~lkls*(1 -~lcurve)*(midinote<~breakpoint);
			klsrl = offset*~rkls*(1-~rcurve)*(midinote>~breakpoint);

			//klsExp = Index.kr(LocalBuf.newFrom(explut),offset.min(32));
			klsExp = Index.kr(explutbuf,offset.min(32));
			klsle = klsExp*~lkls*~lcurve*(midinote<~breakpoint);
			klsre = klsExp*~rkls*~rcurve*(midinote>~breakpoint);


			~alg = algorithms[~algorithm].deepCollect(nil, _.asBinaryDigits(6));
			~alg[1] = ~alg[1] * [
				[~feedback,1,1,1,1,1],
				[1,~feedback,1,1,1,1],
				[1,1,~feedback,1,1,1],
				[1,1,1,~feedback,1,1],
				[1,1,1,1,~feedback,1],
				[1,1,1,1,1,~feedback]
			];

			opamp = ~oplevel + klsll+klsrl+klsle+klsre;
			opamp = opamp.min(-15) + ((~opamd+~amd)*lfo);
			opamp = opamp + ((velocity.ampdb)*~opsensitivity* (1-~alg[0]));


			op = FM7.ar(6.collect{|i| [oppitch[i].midicps,0,opamp[i].dbamp*env[i]]}, ~alg[1]*100);

			// Out
			audio = Mix(op*~alg[0]);

			if(cutoff != 0){
				audio = RLPF.ar(audio, (cutoff+midinote).midicps.min(20000), res);
			};

			audio.isUGen.if{
				Out.ar(out,DirtPan.ar(audio*level, 2, pan));
			};


		}).add;
	}
};


SynthDef(\juno60, {
	var out      = \out.ir(0);
	var pan      = \pan.ir(0);
	var midinote = \midinote.ir(39);
	var cutoff   = \cutoff.ir(36);
	var resonance= \resonance.ir(2);
	var velocity = \velocity.ir(1);
	var noise    = \noise.ir(0.3);
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
	var env, sig, freq;

	lfo = In.kr(~lfobus.index + lfo, 1);

	freq = (midinote + (lfo*vibrato)).midicps;

	env = Env([0,1,susgain,susgain,0], [attack,decay,sustain-(attack+decay),release], -4).ar(
		Done.freeSelf,
		//levelScale: 0.4,
		timeScale: AmpComp.ir(freq, 150, 0.13)
	);


	sig = Saw.ar(freq) + Pulse.ar(freq, (0.5 + (env*0.49*pwm))) + Pulse.ar(freq/2, 0.5, sub);
	sig = sig + WhiteNoise.ar(noise * AmpComp.ir(freq, 238, 0.122));

	cutoff = midinote + cutoff + ((env-susgain)*velocity*12*AmpComp.ir(freq, 200, 0.347)) + (lfocutoff*lfo);
	sig = MoogFF.ar(sig, (cutoff).midicps.min(20000), resonance);
	sig = sig * env * 0.4;

	Out.ar(out, DirtPan.ar(sig, 2, pan));
}).add;


(
SynthDef(\plck, {
	var out      = \out.ir(0);
	var pan      = \pan.ir(0);
	var midinote = \midinote.ir(69);
	var decay    = \decay.ir(4);
	var lfo      = \lfo.ir(0);
	var vibrato  = \vibrato.ir(0);
	var coeff    = \coeff.ir(0.7);
	var cutoff   = \cutoff.ir(90);
	var resonance= \resonance.ir(0.8);

	var sig, ifreq;

	lfo = In.kr(lfo);

	ifreq = (midinote + (lfo*vibrato)).midicps.reciprocal;

	sig = Pluck.ar(
		in:   LPF.ar(WhiteNoise.ar(0.5), (midinote + cutoff).midicps.min(20000)),
		trig: Impulse.kr(0),
		maxdelaytime: (midinote + vibrato).midicps.reciprocal,
		delaytime: ifreq,
		decaytime: decay,
		coef: coeff
	);

	//sig = RLPF.ar(sig, (midinote + cutoff).midicps.min(20000), resonance);

	Out.ar(out, DirtPan.ar(sig, 2, pan));
}).add;
)
