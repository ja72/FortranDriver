using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.CompilerServices;

namespace JA.Fortran
{

    public unsafe partial class FVector :
        System.Collections.ICollection,
        ICollection<double>,
        IFormattable
    {
        public FVector(int size)
        {
            Size=size;
            Data=new double[size];
        }
        internal FVector(double[] data)
        {
            Size=data.GetLength(0);
            Data=data??throw new ArgumentNullException(nameof(data));
        }
        public FVector(int size, Func<int, double> initializer)
            : this(size)
        {
            for (int idx = 0; idx<size; idx++)
            {
                Data[idx]=initializer(idx+1);
            }
        }
        public static FVector FromValues(params double[] values)
        {
            return new FVector(values);
        }

        public static FVector Elemental(int size, int index, double value = 1.0)
        {
            double[] data = new double[size];
            FortranMethods.call_elem_array_v(size, index, value, data);
            return new FVector(data);
        }
        private static int seed = Environment.TickCount;
        public static FVector RandomMinMax(int size, double minValue = 0, double maxValue = 1)
        {
            double[] data = new double[size];
            FortranMethods.call_random_array_v(size, minValue, maxValue, data);
            return new FVector(data);
        }
        public static FVector RandomUniform(int size)
        {
            double[] data = new double[size];
            FortranMethods.call_uniform_array_v(size, ref seed, data);
            return new FVector(data);
        }

        public ref double this[Index index]
        {
            get
            {
                int i = index.GetOffset(Size);
                return ref Data[i-1];
            }
        }

        public FVector this[Range range]
        {
            get
            {
                var (offset, length)=range.GetOffsetAndLength(Size);
                int i1 = offset, i2 = offset + length;
                return Slice(i1, i2);
            }
        }

        public int Size { get; }
        internal double[] Data { get; }


#pragma warning disable IDE0305 // Simplify collection initialization
        public static implicit operator double[](FVector a) => a.ToArray();
#pragma warning restore IDE0305 // Simplify collection initialization
        public static explicit operator FVector(double[] a) => new FVector(a);

        #region Algebra
        public static FVector Round(FVector A, int digits)
        {
            double[] data = new double[A.Size];
            FortranMethods.call_round_array_v(A.Size, A.Data, digits, data);
            return new FVector(data);
        }

        public FMatrix ReShape(int newRows, int newColumns, ElementOrder order)
        {
            int n = Size, k = newRows, l = newColumns;
            double[,] data = new double[l, k];
            FortranMethods.call_reshape_array_vm(n, Data, k, l, order, data);
            return new FMatrix(data);
        }
        public FVector Slice(int startRow, int endRow)
        {
            double[] data = new double[endRow - startRow + 1];
            FortranMethods.call_slice_array_v(Size, Data, startRow, endRow, data);
            return new FVector(data);
        }
        public static double Dot(FVector x, FVector y)
        {
            int size = Math.Min(x.Size, y.Size);
            FortranMethods.call_inner_array_v(size, x.Data, y.Data, out var result);
            return result;
        }
        public static FVector Negate(FVector x)
            => Scale(-1, x);
        public static FVector Add(FVector x, FVector y)
        {
            if (x.Size!=y.Size)
            {
                throw new ArgumentException($"Expecting {x.Size} elements, found {y.Size}.", nameof(y));
            }
            int n = x.Size;
            double[] data = new double[n];
            FortranMethods.call_add_array_v(n, x.Data, y.Data, data);
            return new FVector(data);
        }
        public static FVector Subtract(FVector x, FVector y)
        {
            if (x.Size!=y.Size)
            {
                throw new ArgumentException($"Expecting {x.Size} elements, found {y.Size}.", nameof(y));
            }
            int n = x.Size;
            double[] data = new double[n];
            FortranMethods.call_sub_array_v(n, x.Data, y.Data, data);
            return new FVector(data);
        }
        public static FVector Scale(double x, FVector A)
        {
            int n = A.Size;
            double[] data = new double[n];
            FortranMethods.call_scale_array_v(n, x, A.Data, data);
            return new FVector(data);
        }
        public static FVector Product(FVector x, FMatrix A)
        {
            // | A(n,m) | * | x(m) | = | b(n) |
            if (x.Size!=A.Rows)
            {
                throw new ArgumentException($"Expecting {x.Size} rows, found {A.Rows}.", nameof(A));
            }
            int n = A.Rows, m = A.Columns;
            double[] data = new double[m];
            FortranMethods.call_mul_array_vm(n, m, x.Data, A.Data, data);
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
        public static FVector operator /(FVector a, double x) => Scale(1/x, a);
        public static double operator *(FVector a, FVector b) => Dot(a, b);
        public static FVector operator *(FVector x, FMatrix A) => Product(x, A);
        public static FVector operator /(FVector x, FMatrix A) => A.Solve(x);
        #endregion

        #region Formatting
        public static string DefaultFormat { get; set; } = "g6";

        public override string ToString() => ToString(DefaultFormat);
        public string ToString(string formatting) => ToString(formatting, null);
        public string ToString(string formatting, IFormatProvider formatProvider)
        {
            //double[] data = new double[Size];
            //FortranMethods.call_round_array_v(Size, Data, HelperFunctions.RoundDigits, data);

            StringBuilder sb = new StringBuilder();
            int n = Data.Length;
            string[][] data = [new string[n]];
            for (int i = 0; i<n; i++)
            {
                double f_val = Math.Round(Data[i], HelperFunctions.RoundDigits);
                data[0][i]=f_val.ToString(formatting, formatProvider);
            }
            int maxWidth = data[0].Max((s) => s.Length);
            for (int i = 0; i<n; i++)
            {
                sb.Append('|');
                var text = data[0][i].PadLeft(maxWidth);
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
        public bool Contains(double item) => IndexOf(item)>=0;
        public int IndexOf(double item) => Array.IndexOf(Data, item);
        public IEnumerator<double> GetEnumerator()
        {
            for (int i = 0; i<Data.Length; i++)
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

    }
}
