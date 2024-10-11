using System.Diagnostics.Contracts;

namespace JA.Fortran
{
    public unsafe struct FMatrix2 :
        IFormattable,
        IEquatable<FMatrix2>
    {
        private const int _size = 2;
        private const int _count = _size*_size;
        public static int Size { get; } = _size;
        public static int Count { get; } = _count;

        fixed double _data[_count];

        public FMatrix2(double a11, double a12, double a21, double a22)
        {
            fixed (double* ptr = _data)
            {
                _data[0]=a11;
                _data[1]=a21;
                _data[2]=a12;
                _data[3]=a22;
            }
        }
        public FMatrix2(double[] values, int index = 0)
        {
            Contract.Requires(null!=values);
            Contract.Requires(index+_count==values.Length);
            fixed (double* ptr = _data)
            {
                for (int i = 0; i<_count; i++)
                {
                    _data[i]=values[i];
                }
            }
        }
        public static FMatrix2 Zero { get; } = new FMatrix2(0, 0, 0, 0);
        public static FMatrix2 Identity { get; } = new FMatrix2(1, 0, 0, 1);
        public static FMatrix2 Ones { get; } = new FMatrix2(1, 1, 1, 1);

        public static FMatrix2 Diagonal(double a11, double a22)
            => new FMatrix2(a11, 0, 0, a22);
        public static FMatrix2 Scalar(double a)
            => new FMatrix2(a, 0, 0, a);
        public static FMatrix2 Symmetric(double a11, double a12, double a22)
            => new FMatrix2(a11, a12, a12, a22);
        public static FMatrix2 SkewSymmetric(double a12)
            => new FMatrix2(0, -a12, a12, 0);

        public static explicit operator FMatrix2(double a) => Scalar(a);

        #region Formatting
        public override string ToString() => ToString("g");
        public string ToString(string formatting) => ToString(formatting, null);
        public string ToString(string formatting, IFormatProvider formatProvider)
        {
            string a11 = ((float)_data[0]).ToString(formatting,formatProvider);
            string a21 = ((float)_data[1]).ToString(formatting,formatProvider);

            string a12 = ((float)_data[2]).ToString(formatting,formatProvider);
            string a22 = ((float)_data[3]).ToString(formatting,formatProvider);

            return $"[[{a11},{a12}],[{a21},{a22}]]";
        }
        #endregion

        #region IEquatable Members
        /// <summary>
        /// Equality overrides from <see cref="System.Object"/>
        /// </summary>
        /// <param name="obj">The object to compare this with</param>
        /// <returns>False if object is a different type, otherwise it calls <code>Equals(FMatrix2)</code></returns>
        public override bool Equals(object obj)
        {
            return obj is FMatrix2 vector&&Equals(vector);
        }

        public static bool operator ==(FMatrix2 target, FMatrix2 other) => target.Equals(other);
        public static bool operator !=(FMatrix2 target, FMatrix2 other) => !( target==other );


        /// <summary>
        /// Checks for equality among <see cref="FMatrix2"/> classes
        /// </summary>
        /// <param name="other">The other <see cref="FMatrix2"/> to compare it to</param>
        /// <returns>True if equal</returns>
        public bool Equals(FMatrix2 other)
        {
            return _data[0]==other._data[0]
                &&_data[1]==other._data[1]
                &&_data[2]==other._data[2]
                &&_data[3]==other._data[3];
        }

        /// <summary>
        /// Calculates the hash code for the <see cref="FMatrix2"/>
        /// </summary>
        /// <returns>The int hash value</returns>
        public override int GetHashCode()
        {
            unchecked
            {
                int hc = -1817952719;
                for (int i = 0; i<_count; i++)
                {
                    hc=( -1521134295 )*hc+_data[i].GetHashCode();
                }
                return hc;
            }
        }

        #endregion

        public double A11 => _data[0];
        public double A12 => _data[2];
        public double A21 => _data[1];
        public double A22 => _data[3];

        public readonly double Trace() => FortranMethods.trace_mat2(this);
        public readonly double Determinant() => FortranMethods.determinant_mat2(this);
        public Span<double> AsSpan()
        {
            fixed (double* ptr = _data)
            {
                return new Span<double>(ptr, _count);
            }
        }

        #region Algebra
        public static FMatrix2 Negate(FMatrix2 a) => FortranMethods.mul_scalar_mat2(-1.0, a);
        public static FMatrix2 Scale(double factor, FMatrix2 a) => FortranMethods.mul_scalar_mat2(factor, a);
        public static FMatrix2 Add(FMatrix2 a, FMatrix2 b) => FortranMethods.add_mat2_mat2(a, b);
        public static FMatrix2 Subtract(FMatrix2 a, FMatrix2 b) => FortranMethods.sub_mat2_mat2(a, b);
        public static FMatrix2 Add(double a, FMatrix2 b) => FortranMethods.add_scalar_mat2(a, b);
        public static FMatrix2 Subtract(double a, FMatrix2 b) => FortranMethods.sub_scalar_mat2(a, b);
        public static FMatrix2 Add(FMatrix2 a, double b) => FortranMethods.add_mat2_scalar(a, b);
        public static FMatrix2 Subtract(FMatrix2 a, double b) => FortranMethods.sub_mat2_scalar(a, b);
        public static FVector2 Product(FMatrix2 a, FVector2 b) => FortranMethods.mul_mat2_vec2(a, b);
        public static FVector2 Product(FVector2 a, FMatrix2 b) => FortranMethods.mul_vec2_mat2(a, b);
        public static FMatrix2 Product(FMatrix2 a, FMatrix2 b) => FortranMethods.mul_mat2_mat2(a, b);
        public static FMatrix2 Inner(FMatrix2 a, FMatrix2 b) => FortranMethods.inner_mat2_mat2(a, b);
        public readonly FMatrix2 Transpose() => FortranMethods.transpose_mat2(this);
        public readonly FMatrix2 Inverse() => FortranMethods.inverse_mat2(this);
        public readonly FVector2 Solve(FVector2 b) => FortranMethods.solve_mat2_vec2(this, b);
        public readonly FMatrix2 Solve(FMatrix2 b) => FortranMethods.solve_mat2_mat2(this, b);
        #endregion

        #region Operators
        public static FMatrix2 operator +(FMatrix2 a) => a;
        public static FMatrix2 operator -(FMatrix2 a) => Negate(a);
        public static FMatrix2 operator +(FMatrix2 a, FMatrix2 b) => Add(a, b);
        public static FMatrix2 operator -(FMatrix2 a, FMatrix2 b) => Subtract(a, b);
        public static FMatrix2 operator +(double a, FMatrix2 b) => Add(a, b);
        public static FMatrix2 operator -(double a, FMatrix2 b) => Subtract(a, b);
        public static FMatrix2 operator +(FMatrix2 a, double b) => Add(a, b);
        public static FMatrix2 operator -(FMatrix2 a, double b) => Subtract(a, b);
        public static FMatrix2 operator *(double factor, FMatrix2 a) => Scale(factor, a);
        public static FMatrix2 operator *(FMatrix2 a, double factor) => Scale(factor, a);
        public static FVector2 operator *(FMatrix2 a, FVector2 v) => Product(a, v);
        public static FVector2 operator *(FVector2 v, FMatrix2 a) => Product(v, a);
        public static FMatrix2 operator *(FMatrix2 a, FMatrix2 b) => Product(a, b);
        public static FMatrix2 operator |(FMatrix2 a, FMatrix2 b) => Inner(a, b);
        public static FMatrix2 operator /(FMatrix2 A, double divisor) => Scale(1/divisor, A);
        public static FMatrix2 operator ~(FMatrix2 A) => A.Transpose();
        public static FMatrix2 operator !(FMatrix2 A) => A.Inverse();
        public static FVector2 operator /(FVector2 b, FMatrix2 A) => A.Solve(b);
        public static FMatrix2 operator /(FMatrix2 B, FMatrix2 A) => A.Solve(B);
        #endregion

    }
}
