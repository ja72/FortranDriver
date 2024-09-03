// #define USE_CODE_GEN

namespace FortranDriver
{

    partial class Program
    {

        static void Main(string[] args)
        {
            Console.WriteLine("Calling Fortran from C#");
            Console.WriteLine();
#if USE_CODE_GEN
            LibraryImports.TestFortran();
#else
            DllImports.TestFortran();
#endif
        }

    }
}
