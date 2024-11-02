using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.CompilerServices;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Xml.Linq;

namespace JA.Fortran.Arrays
{

    public unsafe class FVector :
        System.Collections.ICollection,
        ICollection<double>,
        IFormattable
    {

        readonly int _size;
        readonly double[] _data;
        public FVector(int size)
        {
            _size = size;
            _data = new double[size];
        }
        internal FVector(double[] data)
        {
            _size = data.GetLength(0);
            _data = data ?? throw new ArgumentNullException(nameof(data));
        }
        public FVector(int size, Func<int, double> initializer)
            : this(size)
        {
            for (int idx = 0; idx < size; idx++)
            {
                Data[idx] = initializer(idx + 1);
            }
        }
        public static FVector FromValues(params double[] values)
        {
            return new FVector(values);
        }
        public static FVector Zeros(int n)
        {
            double[] data = new double[n];
            call_array_zeros_v(n, data);
            return new FVector(data);
        }
        public static FVector LinearSpace(double x_start, double x_end, int count)
            => linspace(x_start, x_end, count);
        public static FVector Elemental(int size, int index, double value = 1.0)
        {
            double[] data = new double[size];
            call_elem_array_v(size, index, value, data);
            return new FVector(data);
        }

        public static FVector RandomMinMax(int size, double minValue = 0, double maxValue = 1)
        {
            double[] data = new double[size];
            call_random_array_v(size, minValue, maxValue, data);
            return new FVector(data);
        }
        public static FVector RandomUniform(int size)
        {
            double[] data = new double[size];
            call_uniform_array_v(size, ref FortranMethods.seed, data);
            return new FVector(data);
        }
        public int Size => _size;
        internal double[] Data => _data;

        public ref double this[Index index]
        {
            get
            {
                int i = index.GetOffset(Size);
                return ref Data[i - 1];
            }
        }

        public FVector this[Range range]
        {
            get
            {
                var (offset, length) = range.GetOffsetAndLength(Size);
                int i1 = offset, i2 = offset + length;
                return Slice(i1, i2);
            }
        }


#pragma warning disable IDE0305 // Simplify collection initialization
        public static implicit operator double[](FVector a) => a.ToArray();
#pragma warning restore IDE0305 // Simplify collection initialization
        public static explicit operator FVector(double[] a) => new FVector(a);

        public static FMatrix AppendColumns(FVector a, FVector b)
            => FMatrix.FromColumns(a, b);
        public static FMatrix AppendRows(FVector a, FVector b)
            => FMatrix.FromRows(a, b);

        #region Algebra
        public double Norm() => norm_array_v(_size, _data);

        public static FVector Round(FVector A, int digits)
        {
            double[] data = new double[A.Size];
            call_round_array_v(A.Size, A.Data, digits, data);
            return new FVector(data);
        }

        public FMatrix ReShape(int newRows, int newColumns, ElementOrder order)
        {
            int n = Size, k = newRows, l = newColumns;
            double[,] data = new double[l, k];
            call_reshape_array_vm(n, Data, k, l, order, data);
            return new FMatrix(data);
        }
        public FVector Slice(int startRow, int endRow)
        {
            double[] data = new double[endRow - startRow + 1];
            call_slice_array_v(Size, Data, startRow, endRow, data);
            return new FVector(data);
        }
        public static double Dot(FVector x, FVector y)
        {
            int size = Math.Min(x.Size, y.Size);
            call_inner_array_v(size, x.Data, y.Data, out var result);
            return result;
        }
        public static FMatrix Outer(FVector x, FVector y)
        {
            var result = new double[y.Size, x.Size];
            call_outer_array_v(x.Size, y.Size, x, y, result);
            return new FMatrix(result);
        }
        public static FVector Negate(FVector x)
            => Scale(-1, x);
        public static FVector Add(FVector x, FVector y)
        {
            if (x.Size != y.Size)
            {
                throw new ArgumentException($"Expecting {x.Size} elements, found {y.Size}.", nameof(y));
            }
            int n = x.Size;
            double[] data = new double[n];
            call_add_array_v(n, x.Data, y.Data, data);
            return new FVector(data);
        }
        public static FVector Subtract(FVector x, FVector y)
        {
            if (x.Size != y.Size)
            {
                throw new ArgumentException($"Expecting {x.Size} elements, found {y.Size}.", nameof(y));
            }
            int n = x.Size;
            double[] data = new double[n];
            call_sub_array_v(n, x.Data, y.Data, data);
            return new FVector(data);
        }
        public static FVector Scale(double x, FVector A)
        {
            int n = A.Size;
            double[] data = new double[n];
            call_scale_array_v(n, x, A.Data, data);
            return new FVector(data);
        }

        #endregion

        #region Operators
        public static FVector operator +(FVector a) => a;
        public static FVector operator +(FVector a, FVector b) => Add(a, b);
        public static FVector operator -(FVector a) => Negate(a);
        public static FVector operator -(FVector a, FVector b) => Subtract(a, b);
        public static FVector operator *(double x, FVector a) => Scale(x, a);
        public static FVector operator *(FVector a, double x) => Scale(x, a);
        public static FVector operator /(FVector a, double x) => Scale(1 / x, a);
        public static double operator *(FVector a, FVector b) => Dot(a, b);
        public static FVector operator /(FVector x, FMatrix A) => A.Solve(x);
        #endregion

        #region Formatting
        public static string DefaultFormat { get; set; } = "g6";
        public static bool ShowAsTable { get; set; } = true;
        public override string ToString() => ToString(DefaultFormat);
        public string ToString(string formatting) => ToString(formatting, null);
        public string ToString(string formatting, IFormatProvider formatProvider)
        {
            if (ShowAsTable)
            {
                return ToArray().ToTableString(HorizontalAlignment.Right, formatting, formatProvider);
            }
            else
            {
                return ToArray().ToListString(formatting, ",");
            }
        }
        public static string DefaultFormatting { get; set; } = "g6";
        public string ToFixedColumnString(string formatting = null, int width = HelperFunctions.DefaultColumnWidth)
        {
            formatting ??= DefaultFormatting;
            StringBuilder sb = new StringBuilder();
            var data = ToArray();
            for (int i = 0; i < data.Length; i++)
            {
                sb.Append('|');
                var f_val = Math.Round(data[i], HelperFunctions.RoundDigits);
                string text = f_val.ToString(formatting, CultureInfo.CurrentCulture.NumberFormat);
                text = text.PadLeft(width);
                if (text.Length > width)
                {
                    text = new string('*', width);
                }
                sb.Append($" {text}");
                sb.AppendLine(" |");
            }
            sb.AppendLine();
            return sb.ToString();
        }
        #endregion

        #region Collections
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public double[] ToArray() => Data;
        public int Count => Data.Length;
        public Span<double> AsSpan()
        {
            fixed (double* ptr = &Data[0])
            {
                return new Span<double>(ptr, Data.Length);
            }
        }
        public bool Contains(double item) => IndexOf(item) >= 0;
        public int IndexOf(double item) => Array.IndexOf(Data, item);
        public IEnumerator<double> GetEnumerator()
        {
            for (int i = 0; i < Data.Length; i++)
            {
                yield return Data[i];
            }
        }
        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
            => GetEnumerator();
        bool ICollection<double>.IsReadOnly => true;
        void ICollection<double>.Add(double item) => throw new NotSupportedException();
        void ICollection<double>.Clear() => throw new NotSupportedException();
        bool ICollection<double>.Remove(double item) => throw new NotSupportedException();
        bool System.Collections.ICollection.IsSynchronized => false;
        object System.Collections.ICollection.SyncRoot => null;
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        void System.Collections.ICollection.CopyTo(Array array, int index)
            => CopyTo(array as double[], index);
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void CopyTo(double[] array, int index)
            => Array.Copy(Data, 0, array, index, Data.Length);
        #endregion

        #region Fortran API        
        const string libraryName = FortranMethods.libraryName;

        [DllImport(libraryName, EntryPoint = "call_array_zeros_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_array_zeros_v(int size, [Out] double[] A);

        [DllImport(libraryName, EntryPoint = "call_round_array_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_round_array_v(int size, [In] double[] x, int digits, [Out] double[] r);

        [DllImport(libraryName, EntryPoint = "norm_array_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double norm_array_v(int size, [In] double[] A);

        [DllImport(libraryName, EntryPoint = "call_slice_array_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_slice_array_v(int size, [In] double[] A, int start_index, int end_index, [Out] double[] B);

        [DllImport(libraryName, EntryPoint = "call_random_array_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_random_array_v(int size, [In] double minValue, [In] double maxValue, [Out] double[] A);
        [DllImport(libraryName, EntryPoint = "call_uniform_array_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_uniform_array_v(int size, ref int seed, [Out] double[] A);

        [DllImport(libraryName, EntryPoint = "call_elem_array_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_elem_array_v(int size, int index, [In] double x, [Out] double[] A);

        [DllImport(libraryName, EntryPoint = "call_add_array_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_add_array_v(int size, [In] double[] x, [In] double[] y, [Out] double[] z);

        [DllImport(libraryName, EntryPoint = "call_sub_array_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_sub_array_v(int size, [In] double[] x, [In] double[] y, [Out] double[] z);

        [DllImport(libraryName, EntryPoint = "call_scale_array_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_scale_array_v(int size, double x, [In] double[] y, [Out] double[] z);
        [DllImport(libraryName, EntryPoint = "call_reshape_array_vm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_reshape_array_vm(int size, [In] double[] A, int new_rows, int new_columns, ElementOrder order, [Out] double[,] B);

        /// <summary>
        /// Vector inner product.
        /// <code><![CDATA[
        ///     pure subroutine call_inner_array_v(n,x,y,z) bind(c)
        ///     integer, intent(in), value :: n
        ///     real(real64), intent(in) :: x(n), y(n)
        ///     real(real64), intent(out) :: z]]></code>
        /// </summary>
        /// <param name="x">The first vector.</param>
        /// <param name="y">The second vector.</param>
        /// <param name="z">The dot product result.</param>
        [DllImport(libraryName, EntryPoint = "call_inner_array_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_inner_array_v(int size, [In] double[] x, [In] double[] y, [Out] out double z);

        /// <summary>
        /// Generate the outer product of two vectors.
        /// <code><![CDATA[
        /// pure subroutine call_outer_array_v(n,m,x,y,A) bind(c)
        /// !DEC$ ATTRIBUTES DLLEXPORT :: call_outer_array_v
        /// integer, intent(in), value :: n,m
        /// real(real64), intent(in) :: x(n), y(m)
        /// real(real64), intent(out) :: A(n,m)
        /// ]]></code>
        /// </summary>
        /// <param name="n">The size of vector <paramref name="x"/>.</param>
        /// <param name="m">The size of vector <paramref name="y"/>.</param>
        /// <param name="x">The vector x.</param>
        /// <param name="y">The vector y.</param>
        /// <param name="A">The result matrix of size (n,m)</param>
        [DllImport(libraryName, EntryPoint = "call_outer_array_v", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_outer_array_v(int n, int m, [In] double[] x, [In] double[] y, [Out] double[,] A);

        /// <summary>
        /// Create an equally spaced vector of values.
        /// <![CDATA[pure function linspace(x_start, x_end, n_count) result(x)
        /// real(real64), intent(in), value :: x_start, x_end
        /// integer, intent(in), value :: n_count
        /// real(real64) :: x(n_count)]]>
        /// </summary>
        /// <param name="x_start">The start value.</param>
        /// <param name="x_end">The ending value.</param>
        /// <param name="count">The count of values.</param>
        /// <returns>FVector.</returns>
        [DllImport(libraryName, EntryPoint = "linspace", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector linspace(double x_start, double x_end, int count);

        #endregion
    }
}
