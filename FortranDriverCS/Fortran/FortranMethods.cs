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


    }
}
