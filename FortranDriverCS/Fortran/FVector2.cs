using System.Diagnostics.Contracts;
using System.Numerics;

namespace JA.Fortran
{
    public unsafe struct FVector2 :
        IFormattable,
        IEquatable<FVector2>
    {
        private const int _size = 2;
        public static int Size { get; } = _size;

        fixed double _data[_size];

        #region Factory
        public FVector2(double x, double y)
        {
            fixed (double* ptr = _data)
            {
                _data[0]=x;
                _data[1]=y;
            }
        }
        public FVector2(double[] values, int index = 0)
        {
            Contract.Requires(null!=values);
            Contract.Requires(index+_size==values.Length);
            fixed (double* ptr = _data)
            {
                for (int i = 0; i<_size; i++)
                {
                    _data[i]=values[i];
                }
            }
        }
        public static FVector2 Zero { get; } = new FVector2(0, 0);
        public static FVector2 EX { get; } = new FVector2(1, 0);
        public static FVector2 EY { get; } = new FVector2(0, 1);
        public static FVector2 Ones { get; } = new FVector2(1, 1);
        #endregion

        #region Properties
        public double X => _data[0];
        public double Y => _data[1];

        public Span<double> AsSpan()
        {
            fixed (double* ptr = _data)
            {
                return new Span<double>(ptr, _size);
            }
        }

        public ref double this[int index] => ref _data[index];
        #endregion

        #region Formatting
        public override string ToString() => ToString("g");
        public string ToString(string formatting) => ToString(formatting, null);
        public string ToString(string formatting, IFormatProvider formatProvider)
        {
            string x = Math.Round(_data[0], HelperFunctions.RoundDigits).ToString(formatting, formatProvider);
            string y = Math.Round(_data[1], HelperFunctions.RoundDigits).ToString(formatting, formatProvider);

            return $"[{x},{y}]";
        }
        #endregion

        #region IEquatable Members
        /// <summary>
        /// Equality overrides from <see cref="System.Object"/>
        /// </summary>
        /// <param name="obj">The object to compare this with</param>
        /// <returns>False if object is a different type, otherwise it calls <code>Equals(FVector2)</code></returns>
        public override bool Equals(object obj)
        {
            return obj is FVector2 vector&&Equals(vector);
        }

        public static bool operator ==(FVector2 target, FVector2 other) => target.Equals(other);
        public static bool operator !=(FVector2 target, FVector2 other) => !( target==other );


        /// <summary>
        /// Checks for equality among <see cref="FVector2"/> classes
        /// </summary>
        /// <param name="other">The other <see cref="FVector2"/> to compare it to</param>
        /// <returns>True if equal</returns>
        public bool Equals(FVector2 other)
        {
            return _data[0]==other._data[0]
                &&_data[1]==other._data[1];
        }

        /// <summary>
        /// Calculates the hash code for the <see cref="FVector2"/>
        /// </summary>
        /// <returns>The int hash value</returns>
        public override int GetHashCode()
        {
            unchecked
            {
                int hc = -1817952719;
                for (int i = 0; i<_size; i++)
                {
                    hc=( -1521134295 )*hc+_data[i].GetHashCode();
                }
                return hc;
            }
        }

        #endregion

        #region Algebra
        public static FVector2 Negate(FVector2 A) => FortranMethods.mul_scalar_vec2(-1.0, A);
        public static FVector2 Scale(double factor, FVector2 A) => FortranMethods.mul_scalar_vec2(factor, A);
        public static FVector2 Add(FVector2 A, FVector2 B) => FortranMethods.add_vec2_vec2(A, B);
        public static FVector2 Subtract(FVector2 A, FVector2 B) => FortranMethods.sub_vec2_vec2(A, B);
        public static double Dot(FVector2 A, FVector2 B) => FortranMethods.inner_vec2_vec2(A, B);
        public static FMatrix2 Outer(FVector2 a, FVector2 b) => FortranMethods.outer_vec2_vec2(a, b);
        public static double Cross(FVector2 a, FVector2 b) => FortranMethods.cross_vec2_vec2(a, b);
        public static FVector2 Cross(double a, FVector2 b) => FortranMethods.cross_scalar_vec2(a, b);
        public static FVector2 Cross(FVector2 a, double b) => FortranMethods.cross_vec2_scalar(a, b);
        #endregion

        #region Operators
        public static FVector2 operator +(FVector2 a) => a;
        public static FVector2 operator +(FVector2 a, FVector2 b) => Add(a, b);
        public static FVector2 operator -(FVector2 a) => Negate(a);
        public static FVector2 operator -(FVector2 a, FVector2 b) => Subtract(a, b);
        public static FVector2 operator *(double a, FVector2 b) => Scale(a, b);
        public static FVector2 operator *(FVector2 a, double b) => Scale(b, a);
        public static FVector2 operator /(FVector2 a, double b) => Scale(1/b, a);
        public static double operator |(FVector2 a, FVector2 b) => Dot(a, b);
        #endregion

    }
}
