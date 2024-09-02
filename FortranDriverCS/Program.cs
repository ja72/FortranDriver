namespace FortranDriver
{

    partial class Program
    {

        static void Main(string[] args)
        {
            Console.WriteLine("Calling Fortran from C#");
            Console.WriteLine();
            UnmanagedImports.TestFortran();
            UnmanagedLibrary.TestFortran();
        }

    }
}
