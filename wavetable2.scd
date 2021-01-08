~sound = Buffer.read(s, "/home/sirbu/downloads/427567__maria-mannone__flute.wav");



(
var nsignals = 10, size=2048;
~signals = Array.newClear(nsignals);
nsignals.do{ |i|
	~sound.getn(i*size, size, { |arr|
		~signals[i] = Signal.newFrom(arr);
	});
};
)

(
var signals, buffers, bufPerOctave=2, midiLo = 0, midiHi = 98, size=2048;
var bufPerTable;
var imag, cosTable;


signals = ~signals;

bufPerTable =
	((midiHi-midiLo) / (12/bufPerOctave)).asInt;
buffers = Buffer.allocConsecutive(
	bufPerTable * signals.size,
	s,
	size * 2,
);

b = buffers;


imag = Signal.newClear(size);
cosTable = Signal.fftCosTable(size);

signals.do{|real, j|
	var complex;

	// real = Signal.newClear(size);
	// real.waveFill(func, 0, 1);
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

)

b[7].plot
(
{
	var sig;
	sig = VOsc.ar(b[0].bufnum + MouseX.kr(0,10), 20);
	(sig*0.2).dup
}.play
)