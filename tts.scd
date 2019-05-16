//SuperDirt.start;

(
var ttsEvent;
var ttsBuffers = Dictionary.new;
var ttsplay;

ttsplay = { |dirtEvent, buffer, envir|
	var baseFreq = 60.midicps;

	dirtEvent.event.use({
		~buffer = buffer.bufnum;
		~instrument = ~dirt.soundLibrary.instrumentForBuffer(buffer);
		~numFrames = buffer.numFrames;
		~bufNumChannels = buffer.numChannels;
		~unitDuration = { buffer.duration * baseFreq / ~freq.value };
		~hash = buffer.identityHash;
		~note = 0;

		dirtEvent.calcTimeSpan;
		dirtEvent.playSynths;
	});
};

ttsEvent = (
	play: { |dirtEvent|
		if(ttsBuffers[~text].isNil) {
			var file;
			file = PathName.tmp +/+ ~text.asString ++ ".wav";
			"echo \"%\" | text2wave -o \"%\"".format(~text.asString.escapeChar($"), file).systemCmd;
			ttsBuffers[~text] = Buffer.read(~server, file, 0.02*48000
				//action: { |buffer| ttsplay.value(dirtEvent, buffer) }
			);
			//~server.sync;
		}{
			ttsplay.value(dirtEvent, ttsBuffers[~text]);
		}
	}
);
~dirt.soundLibrary.addSynth(\tts, ttsEvent);
)
