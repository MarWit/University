using System;

namespace SIDious {
    /**
     * Interfejs udostępniający metodę pozwalający na zmapowania kawałka tablicy.
     */
    interface IMappable<T> {
        void attachMemory( ArrayChunk<T> memory );
    }
}
