using System;
using System.Text;
using System.Runtime.InteropServices;

/**
 * Klasa-wrapper na funkcje PulseAudio.
 * Klasa udostępniająca API do obsługi funkcji z libpulse-simple
 */
class PulseAudio {
    public static int PA_STREAM_NODIRECTION = 0;
    public static int PA_STREAM_PLAYBACK = 1;
    public static int PA_STREAM_RECORD = 2;
    public static int PA_STREAM_UPLOAD = 3;

    [StructLayout(LayoutKind.Sequential)]
    public struct pa_sample_spec {
        public int format;
        public uint rate;
        public byte channels;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct pa_channel_map {
        public byte channels;
        public int map;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct pa_buffer_attr {
        public uint maxlenght;
        public uint tlength;
        public uint prebuf;
        public uint minreq;
        public uint fragsize;
    }


    [DllImport("libpulse-simple.so")]
    public static extern IntPtr pa_simple_new(
                                        String server,
                                        String name,
                                        int dir,
                                        String dev,
                                        String stream_name,
                                        ref pa_sample_spec ss,
                                        IntPtr map,
                                        IntPtr attr,
                                        IntPtr error );

    [DllImport("libpulse-simple.so")]
    public static extern int pa_simple_write(
                                        IntPtr s,
                                        short[] data,
                                        uint bytes,
                                        ref int error );

    [DllImport("libpulse-simple.so")]
    public static extern void pa_simple_destroy( IntPtr s );
}
