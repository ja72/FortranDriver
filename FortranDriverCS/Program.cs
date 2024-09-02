using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace FortranDriver
{
    partial class Program
    {

        // Delegate type.
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate void ActionRefInt(ref int progress, ref int count); // Important the int is passed by ref (else we could use built-in Action<T> instead of delegate).

        // Delegate variable.
        public static ActionRefInt callbackHandler;

        static void Main(string[] args)
        {
            Console.WriteLine("Calling Fortran from C#");

            callbackHandler = new ActionRefInt(OnUpdateProgress);

            int n = 10;
            double[,] A = BuildArray(n, (i,j)=> i==j ? i+1 : 0);
            ShowArray(A, 5);

#if USE_CODE_GEN
            DoWork(ref n, ref A[0,0], myProg.callbackHandler);
#else
            DoWork(ref n, A, callbackHandler);
#endif

            ShowArray(A, 5);
            }

        static double[,] BuildArray(int n, Func<int,int,double> init = null)
        {
            var A = new double[n, n];
            for (int i = 0; i < n; i++)
            {
                for (int j = 0; j < n; j++)
                {
                    if (init==null)
                    {
                        A[i, j] = 0.0;
                    }
                    else
                    {
                        A[i, j] = init(i, j);
                    }
                }
            }

            return A;
        }

        public static void ShowArray(double[,] A, int width=11, string formatting = "g4")
        {
            int rows = A.GetLength(0), cols = A.GetLength(1);

            for (int i = 0; i < rows; i++)
            {
                Console.Write("|");
                for (int j = 0; j < cols; j++)
                {
                    string text = A[i, j].ToString(formatting).PadLeft(width);
                    if (text.Length>width)
                    {
                        text = text.Substring(0, width-1) + "…";
                    }
                    Console.Write($" {text}");
                }
                Console.WriteLine(" |");
            }
            Console.WriteLine();
        }

        public static void OnUpdateProgress(ref int progress, ref int count)
        {
            Console.WriteLine($" Step: \t\t {progress,2} of {count,2}");
        }

#if USE_CODE_GEN
        [LibraryImport("FortranDriverDLL.dll", EntryPoint = "DoWork", StringMarshalling = StringMarshalling.Utf8)]
        [UnmanagedCallConv( CallConvs = new[] { typeof(CallConvCdecl) })]
        static partial void DoWork(ref int n, ref double A, [MarshalAs(UnmanagedType.FunctionPtr)] ActionRefInt callBack);
#else
        // Fortran DLL interface.
        [DllImport("FortranDriverDLL.dll", EntryPoint = "DoWork", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        public static extern void DoWork(ref int n, [In, Out] double[,] A, [MarshalAs(UnmanagedType.FunctionPtr)] ActionRefInt callBack);       
#endif
    }
}
