using System.Drawing.Drawing2D;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

using Windows.Storage.FileProperties;

using static System.Runtime.InteropServices.JavaScript.JSType;
using static JA.Program;

namespace JA.Fortran
{
    internal static partial class FortranMethods
    {
        // NOTE: Fortran methods declared with `DllImport()`. Consider use the newer `LibraryImport()`
        // delcaration instead. Use a ref to first element instead of passing a 2D array.
#if DEBUG
        public const string libraryName = "FortranDriverDLL_d";
        public static int seed = 123456789;
#else
        public const string libraryName = "FortranDriverDLL";
        public static int seed = Environment.TickCount;
#endif

        

        #region Interop Methods

        /// <summary>
        /// Fortran DLL call to manipulate matrix <paramref name="A"/>
        /// </summary>
        /// <param name="rows">The n.</param>
        /// <param name="columns">The n.</param>
        /// <param name="A">The matrix. Fortran requires <paramref name="A"/> to be column major 
        /// and C# supplies a row major matrix by default.</param>
        /// <param name="callBack">The call-back function to report on progress.</param>
        [DllImport(libraryName, EntryPoint = "call_test_dowork", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_test_dowork(int rows, int columns, [In, Out] double[,] A, [MarshalAs(UnmanagedType.FunctionPtr)] ActionRefInt callBack);

        [DllImport(libraryName, EntryPoint = "call_quat_test_all", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_quat_test_all();
        #endregion

    }
}
