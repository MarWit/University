using System;
using NAudio.Wave;

namespace SIDious {
    partial class SID {
        class WaveOutput : IWavePlayer {
            public void Init( IWaveProvider waveProvider ) {

            }

            public void Play() {}
            public void Stop() {}
            public void Pause() {}
            public PlaybackState PlaybackState { get; }
            public float Volume { get; set; }
            public event EventHandler<StoppedEventArgs> PlaybackStopped;
        }
    }
}
