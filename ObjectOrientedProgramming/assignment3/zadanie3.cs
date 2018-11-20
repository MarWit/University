using System;

namespace Zadanie3 {
    public class TimeNTon {
        const int N = 5;

        static int C = 0;
        static TimeNTon[ ] instances = new TimeNTon[ N ];
        static bool locked = false;

        private int idx;

        TimeNTon( int idx ) { this . idx = idx; }

        public static TimeNTon Instance( ) {
            DateTime dt = DateTime.Now;

            if(
                dt . DayOfWeek == DayOfWeek . Friday &&
                dt . Hour >= 10 && dt . Hour <= 12
            ) {
                if( C >= N ) {
                    C = 0; locked = true;
                }

                if( ! locked )
                    instances[ C ] = new TimeNTon( C );

                return instances[ C ++ ];
            } else {
                if( C == 0 && ! locked )
                    instances[ 0 ] = new TimeNTon( C );

                return instances[ 0 ];
            }
        }

        public override string ToString( ) {
            return String.Format( "TimeNTon #{0}", idx );
        }
    }
}
