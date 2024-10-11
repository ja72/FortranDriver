using System.Diagnostics;

namespace JA.UI
{
    public class FpsCounter
    {
        public FpsCounter()
        {
            PrevFrame = 0;
            Frames = 0;
            PollOverFrames = 100;
            Clock = Stopwatch.StartNew();
        }
        /// <summary>
        /// Use this method to poll the FPS counter
        /// </summary>
        /// <returns>The last measured FPS</returns>
        public float Measure()
        {
            Frames++;
            PrevFrame++;
            var dt = Clock.Elapsed.TotalSeconds;

            if (PrevFrame > PollOverFrames || dt > PollOverFrames / 50)
            {
                LastFps = (float)(PrevFrame / dt);
                PrevFrame = 0;
                Clock.Restart();
            }

            return LastFps;
        }
        public float LastFps { get; private set; }
        public long Frames { get; private set; }
        private Stopwatch Clock { get; }
        private int PrevFrame { get; set; }
        /// <summary>
        /// The number of frames to average to get a more accurate frame count.
        /// The higher this is the more stable the result, but it will upadte
        /// slower. The lower this is, the more chaotic the result of <see cref="Measure()"/>
        /// but it will get a new result sooner. Default is 100 frames.
        /// </summary>
        public int PollOverFrames { get; set; }
    }
}
