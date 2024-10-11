using System.Diagnostics.Contracts;

namespace JA.Fortran
{
    public unsafe struct FMatrix3 :
        IFormattable,
        IEquatable<FMatrix3>
    {
        private const int _size = 3;
        private const int _count = _size*_size;
        public static int Size { get; } = _size;
        public static int Count { get; } = _count;

        fixed double _data[_count];

        public FMatrix3(
            double a11, double a12, double a13,
            double a21, double a22, double a23,
            double a31, double a32, double a33)
        {
            fixed (double* ptr = _data)
            {
                _data[0+0]=a11;
                _data[0+1]=a21;
                _data[0+2]=a31;
                _data[3+0]=a12;
                _data[3+1]=a22;
                _data[3+2]=a32;
                _data[6+0]=a13;
                _data[6+1]=a23;
                _data[6+2]=a33;
            }
        }
        public FMatrix3(double[] values, int index = 0)
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
        public static FMatrix3 Zero { get; } = new FMatrix3(0, 0, 0, 0, 0, 0, 0, 0, 0);
        public static FMatrix3 Identity { get; } = new FMatrix3(1, 0, 0, 0, 1, 0, 0, 0, 1);
        public static FMatrix3 Ones { get; } = new FMatrix3(1, 1, 1, 1, 1, 1, 1, 1, 1);

        public static FMatrix3 Diagonal(double a11, double a22, double a33)
            => new FMatrix3(a11, 0, 0, 0, a22, 0, 0, 0, a33);
        public static FMatrix3 Scalar(double a)
            => new FMatrix3(a, 0, 0, 0, a, 0, 0, 0, a);
        public static FMatrix3 Symmetric(double a11, double a12, double a13, double a22, double a23, double a33)
            => new FMatrix3(a11, a12, a12, a12, a22, a23, a13, a23, a33);
        public static FMatrix3 SkewSymmetric(double a12, double a13, double a23)
            => new FMatrix3(0, a12, a13, a12, 0, a23, a13, a23, 0);

        public static explicit operator FMatrix3(double a) => Scalar(a);

        #region Formatting
        public override string ToString() => ToString("g");
        public string ToString(string formatting) => ToString(formatting, null);
        public string ToString(string formatting, IFormatProvider formatProvider)
        {
            string a11 = Math.Round(_data[0+0], HelperFunctions.RoundDigits).ToString(formatting,formatProvider);
            string a21 = Math.Round(_data[0+1], HelperFunctions.RoundDigits).ToString(formatting,formatProvider);
            string a31 = Math.Round(_data[0+2], HelperFunctions.RoundDigits).ToString(formatting,formatProvider);
                                    
            string a12 = Math.Round(_data[3+0], HelperFunctions.RoundDigits).ToString(formatting,formatProvider);
            string a22 = Math.Round(_data[3+2], HelperFunctions.RoundDigits).ToString(formatting,formatProvider);
            string a32 = Math.Round(_data[3+3], HelperFunctions.RoundDigits).ToString(formatting,formatProvider);
                                    
            string a13 = Math.Round(_data[6+0], HelperFunctions.RoundDigits).ToString(formatting,formatProvider);
            string a23 = Math.Round(_data[6+2], HelperFunctions.RoundDigits).ToString(formatting,formatProvider);
            string a33 = Math.Round(_data[6+3], HelperFunctions.RoundDigits).ToString(formatting,formatProvider);

            return $"[[{a11},{a12},{a13}],[{a21},{a22},{a23}],[{a31},{a32},{a33}]]";
        }
        #endregion

        #region IEquatable Members
        /// <summary>
        /// Equality overrides from <see cref="System.Object"/>
        /// </summary>
        /// <param name="obj">The object to compare this with</param>
        /// <returns>False if object is a different type, otherwise it calls <code>Equals(FMatrix3)</code></returns>
        public override bool Equals(object obj)
        {
            return obj is FMatrix3 vector&&Equals(vector);
        }

        public static bool operator ==(FMatrix3 target, FMatrix3 other) => target.Equals(other);
        public static bool operator !=(FMatrix3 target, FMatrix3 other) => !( target==other );


        /// <summary>
        /// Checks for equality among <see cref="FMatrix3"/> classes
        /// </summary>
        /// <param name="other">The other <see cref="FMatrix3"/> to compare it to</param>
        /// <returns>True if equal</returns>
        public bool Equals(FMatrix3 other)
        {
            return _data[0]==other._data[0]
                &&_data[1]==other._data[1]
                &&_data[2]==other._data[2]
                &&_data[3]==other._data[3]
                &&_data[4]==other._data[4]
                &&_data[5]==other._data[5]
                &&_data[6]==other._data[6]
                &&_data[7]==other._data[7]
                &&_data[8]==other._data[8];
        }

        /// <summary>
        /// Calculates the hash code for the <see cref="FMatrix3"/>
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

        public double A11 => _data[0+0];
        public double A12 => _data[3+0];
        public double A13 => _data[6+0];
        public double A21 => _data[0+1];
        public double A22 => _data[3+1];
        public double A23 => _data[6+1];
        public double A31 => _data[0+2];
        public double A32 => _data[3+2];
        public double A33 => _data[6+2];

        public readonly double Trace() => FortranMethods.trace_mat3(this);
        public readonly double Determinant() => FortranMethods.determinant_mat3(this);
        public Span<double> AsSpan()
        {
            fixed (double* ptr = _data)
            {
                return new Span<double>(ptr, _count);
            }
        }

        #region Algebra
        public static FMatrix3 Negate(FMatrix3 a) => FortranMethods.mul_scalar_mat3(-1.0, a);
        public static FMatrix3 Scale(double factor, FMatrix3 a) => FortranMethods.mul_scalar_mat3(factor, a);
        public static FMatrix3 Add(FMatrix3 a, FMatrix3 b) => FortranMethods.add_mat3_mat3(a, b);
        public static FMatrix3 Subtract(FMatrix3 a, FMatrix3 b) => FortranMethods.sub_mat3_mat3(a, b);
        public static FMatrix3 Add(double a, FMatrix3 b) => FortranMethods.add_scalar_mat3(a, b);
        public static FMatrix3 Subtract(double a, FMatrix3 b) => FortranMethods.sub_scalar_mat3(a, b);
        public static FMatrix3 Add(FMatrix3 a, double b) => FortranMethods.add_mat3_scalar(a, b);
        public static FMatrix3 Subtract(FMatrix3 a, double b) => FortranMethods.sub_mat3_scalar(a, b);
        public static FVector3 Product(FMatrix3 a, FVector3 b) => FortranMethods.mul_mat3_vec3(a, b);
        public static FVector3 Product(FVector3 a, FMatrix3 b) => FortranMethods.mul_vec3_mat3(a, b);
        public static FMatrix3 Product(FMatrix3 a, FMatrix3 b) => FortranMethods.mul_mat3_mat3(a, b);
        public static FMatrix3 Inner(FMatrix3 a, FMatrix3 b) => FortranMethods.inner_mat3_mat3(a, b);
        public readonly FMatrix3 Transpose() => FortranMethods.transpose_mat3(this);
        public readonly FMatrix3 Inverse() => FortranMethods.inverse_mat3(this);
        public readonly FVector3 Solve(FVector3 b) => FortranMethods.solve_mat3_vec3(this, b);
        public readonly FMatrix3 Solve(FMatrix3 b) => FortranMethods.solve_mat3_mat3(this, b);
        #endregion

        #region Operators
        public static FMatrix3 operator +(FMatrix3 a) => a;
        public static FMatrix3 operator -(FMatrix3 a) => Negate(a);
        public static FMatrix3 operator +(FMatrix3 a, FMatrix3 b) => Add(a, b);
        public static FMatrix3 operator -(FMatrix3 a, FMatrix3 b) => Subtract(a, b);
        public static FMatrix3 operator +(double a, FMatrix3 b) => Add(a, b);
        public static FMatrix3 operator -(double a, FMatrix3 b) => Subtract(a, b);
        public static FMatrix3 operator +(FMatrix3 a, double b) => Add(a, b);
        public static FMatrix3 operator -(FMatrix3 a, double b) => Subtract(a, b);
        public static FMatrix3 operator *(double factor, FMatrix3 a) => Scale(factor, a);
        public static FMatrix3 operator *(FMatrix3 a, double factor) => Scale(factor, a);
        public static FVector3 operator *(FMatrix3 a, FVector3 v) => Product(a, v);
        public static FVector3 operator *(FVector3 v, FMatrix3 a) => Product(v, a);
        public static FMatrix3 operator *(FMatrix3 a, FMatrix3 b) => Product(a, b);
        public static FMatrix3 operator |(FMatrix3 a, FMatrix3 b) => Inner(a, b);
        public static FMatrix3 operator /(FMatrix3 A, double divisor) => Scale(1/divisor, A);
        public static FMatrix3 operator ~(FMatrix3 A) => A.Transpose();
        public static FMatrix3 operator !(FMatrix3 A) => A.Inverse();
        public static FVector3 operator /(FVector3 b, FMatrix3 A) => A.Solve(b);
        public static FMatrix3 operator /(FMatrix3 B, FMatrix3 A) => A.Solve(B);
        #endregion
    }
}
