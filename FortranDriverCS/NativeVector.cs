using System.Text;
using System.Globalization;
using System.Runtime.InteropServices;
using static System.Runtime.InteropServices.JavaScript.JSType;

namespace FortranDriver
{
    public unsafe class NativeVector :
        IFormattable
    {
        public NativeVector(int size)
        {
            this.Size=size;
            this.Data=new double[size];
        }
        internal NativeVector(double[] data)
        {
            this.Size=data.GetLength(0);
            this.Data=data??throw new ArgumentNullException(nameof(data));
        }
        public NativeVector(int size, Func<int, double> initializer)
            : this(size)
        {
            for (int idx = 0; idx < size; idx++)
            {
                Data[idx] = initializer(idx+1);
            }
        }

        public static NativeVector Elemental(int size, int index, double value = 1.0)
        {
            double[] data = new double[size];
            NativeVectorMethods.array_elem_v(size, index, value, data);
            return new NativeVector(data);
        }
        private static int seed = Environment.TickCount;
        public static NativeVector RandomMinMax(int size, double minValue = 0, double maxValue = 1)
        {
            double[] data = new double[size];
            NativeVectorMethods.array_rand_v(size, minValue, maxValue, data);
            return new NativeVector(data);
        }
        public static NativeVector RandomUniform(int size)
        {
            double[] data = new double[size];
            NativeVectorMethods.array_uniform_v(size, ref seed, data);
            return new NativeVector(data);
        }

        public ref double this[Index index]
        {
            get
            {
                int i = index.GetOffset(Size);
                return ref Data[i-1];
            }
        }

        public NativeVector this[Range range]
        {
            get
            {
                var (offset, length)= range.GetOffsetAndLength(Size);
                int i1 = offset, i2 = offset + length;
                return Slice(this, i1, i2);
            }
        }

        public int Size { get; }
        public int Count { get => Size; }
        internal double[] Data { get; }

        public Span<double> AsSpan()
        {
            fixed (double* ptr = &Data[0])
            {
                return new Span<double>(ptr, Data.Length);
            }
        }

        public double[] ToArray() => Data;

        public static implicit operator double[](NativeVector a) => a.ToArray();
        public static explicit operator NativeVector(double[] a) => new NativeVector(a);

        #region Algebra
        public static NativeVector Slice(NativeVector A, int startRow, int endRow)
        {
            double[] data = new double[endRow-startRow+1];
            NativeVectorMethods.array_slice_v(A.Size, A, startRow, endRow, data);
            return new NativeVector(data);
        }
        public static double Dot(NativeVector x, NativeVector y)
        {
            int size = Math.Min(x.Size, y.Size);
            NativeVectorMethods.array_dot_v(size, x.Data, y.Data, out var result);
            return result;
        }
        public static NativeVector Negate(NativeVector x)
            => Scale(-1, x);
        public static NativeVector Add(NativeVector x, NativeVector y)
        {
            if (x.Size != y.Size)
            {
                throw new ArgumentException($"Expecting {x.Size} elements, found {y.Size}.", nameof(y));
            }
            int n = x.Size;
            double[] data = new double[n];
            NativeVectorMethods.array_add_v(n, x.Data, y.Data, data);
            return new NativeVector(data);
        }
        public static NativeVector Subtract(NativeVector x, NativeVector y)
        {
            if (x.Size != y.Size)
            {
                throw new ArgumentException($"Expecting {x.Size} elements, found {y.Size}.", nameof(y));
            }
            int n = x.Size;
            double[] data = new double[n];
            NativeVectorMethods.array_subtract_v(n, x.Data, y.Data, data);
            return new NativeVector(data);
        }
        public static NativeVector Scale(double x, NativeVector A)
        {
            int n = A.Size;
            double[] data = new double[n];
            NativeVectorMethods.array_scale_v(n, x, A.Data, data);
            return new NativeVector(data);
        }
        public static NativeVector Product(NativeVector x, NativeMatrix A)
        {
            // | A(n,m) | * | x(m) | = | b(n) |
            if (x.Size != A.Rows)
            {
                throw new ArgumentException($"Expecting {x.Size} rows, found {A.Rows}.", nameof(A));
            }
            int n = A.Rows, m = A.Columns;
            double[] data = new double[m];
            NativeVectorMethods.array_product_vm(n, m, x.Data, A.Data, data);
            return new NativeVector(data);
        }
        public static NativeVector Solve(NativeMatrix A, NativeVector b)
        {
            // | A(n,m) | * | x(m) | = | b(n) |
            if (b.Size != A.Rows)
            {
                throw new ArgumentException($"Expecting {A.Rows} elements, found {b.Size}.", nameof(b));
            }
            int n = A.Rows, m = A.Columns;
            double[] data = new double[m];
            NativeVectorMethods.array_solve_mv(n, m, A.Data, b.Data, data);
            return new NativeVector(data);
        }

        #endregion

        #region Operators
        public static NativeVector operator +(NativeVector a) => a;
        public static NativeVector operator +(NativeVector a, NativeVector b) => Add(a, b);
        public static NativeVector operator -(NativeVector a) => Negate(a);
        public static NativeVector operator -(NativeVector a, NativeVector b) => Subtract(a, b);
        public static NativeVector operator *(double x, NativeVector a) => Scale(x, a);
        public static NativeVector operator *(NativeVector a, double x) => Scale(x, a);
        public static NativeVector operator /(NativeVector a, double x) => Scale(1 / x, a);
        public static double operator *(NativeVector a, NativeVector b) => Dot(a, b);
        public static NativeVector operator *(NativeVector x, NativeMatrix A) => Product(x, A);
        public static NativeVector operator /(NativeVector x, NativeMatrix A) => Solve(A, x);
        #endregion

        #region Formatting
        public static string DefaultFormat { get; set; } = "g6";

        public override string ToString() => ToString(DefaultFormat);
        public string ToString(string formatting) => ToString(formatting, null);
        public string ToString(string formatting, IFormatProvider formatProvider)
        {
            const int width = 11;
            StringBuilder sb = new StringBuilder();
            int n = Data.Length;
            for (int i = 0; i < n; i++)
            {
                sb.Append('|');
                var obj = Data[i];
                string text = obj.ToString(formatting, formatProvider);
                text  = text.PadLeft(width);
                if (text.Length>width)
                {
                    text = $"{text.Substring(0, width-1)}…";
                }
                sb.Append($" {text}");
                sb.AppendLine(" |");
            }
            sb.AppendLine();
            return sb.ToString();            
        }
        #endregion

        #region Fortran Methods
        // NOTE: Fortran methods declared with `DllImport()`. Consider use the newer `LibraryImport()`
        //       delcaration instead. Use a ref to first element instead of passing a 2D array.

        internal static class NativeVectorMethods
        {
            const string libraryName = "FortranDriverDLL";

            

            [DllImport(libraryName, EntryPoint = "array_norm_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_norm_v(int size, [In] double[] A, ref double s);

            [DllImport(libraryName, EntryPoint = "array_slice_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_slice_v(int size, [In] double[] A, int start_index, int end_index, [Out] double[] B);

            [DllImport(libraryName, EntryPoint = "array_rand_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_rand_v(int size, [In] double minValue, [In] double maxValue, [Out] double[] A);
            [DllImport(libraryName, EntryPoint = "array_uniform_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_uniform_v(int size, ref int seed, [Out] double[] A);

            [DllImport(libraryName, EntryPoint = "array_elem_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_elem_v(int size, int index, [In] double x, [Out] double[] A);

            [DllImport(libraryName, EntryPoint = "array_add_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_add_v(int size, [In] double[] x, [In] double[] y, [Out] double[] z);

            [DllImport(libraryName, EntryPoint = "array_subtract_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_subtract_v(int size, [In] double[] x, [In] double[] y, [Out] double[] z);

            [DllImport(libraryName, EntryPoint = "array_scale_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_scale_v(int size, double x, [In] double[] y, [Out] double[] z);

            /// <summary>
            /// Fortran DLL call to matrix multiply.
            /// </summary>
            /// <param name="x">The first vector.</param>
            /// <param name="y">The second vector.</param>
            /// <param name="z">The dot product result.</param>
            [DllImport(libraryName, EntryPoint = "array_dot_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_dot_v(int size, [In] double[] x, [In] double[] y, [Out] out double z);

            /// <summary>
            /// Fortran DLL call to matrix multiply <code>b=x*A</code>.
            /// </summary>
            /// <param name="x">The known vector</param>
            /// <param name="A">The coefficient matrix.</param>
            /// <param name="b">The result vector</param>
            [DllImport(libraryName, EntryPoint = "array_product_vm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_product_vm(int rows, int columns, [In] double[] x, [In] double[,] A, [Out] double[] b);

            /// <summary>
            /// Fortran DLL call to solve the linear system of equations <code>A*x=b</code> for <paramref name="x"/>.
            /// </summary>
            /// <param name="A">The coefficient matrix.</param>
            /// <param name="b">The known vector</param>
            /// <param name="x">The unknown vector</param>
            [DllImport(libraryName, EntryPoint = "array_solve_mv", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
            public static extern void array_solve_mv(int rows, int columns, [In] double[,] A, [In] double[] b, [Out] double[] x);

        }

        #endregion

        #region Testing

        #endregion

    }
}
