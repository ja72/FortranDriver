using System.Diagnostics.Contracts;

namespace JA.Fortran
{
    public unsafe struct FVector3 : 
        IFormattable,
        IEquatable<FVector3>
    {
        private const int _size = 3;
        public static int Size { get; } = _size;

        fixed double _data[_size];

        #region Factory
        public FVector3(double x, double y, double z)
        {
            fixed (double* ptr = _data)
            {
                ptr[0]=x;
                ptr[1]=y;
                ptr[2]=z;
            }

        }
        public FVector3(double[] values, int index = 0)
        {
            Contract.Requires(null!=values);
            Contract.Requires(index+_size<=values.Length);
            fixed (double* ptr = _data)
            {
                for (int i = 0; i<_size; i++)
                {
                    _data[i]=values[i];
                }
            }
        }
        public static FVector3 Zero { get; } = new FVector3(0, 0, 0);
        public static FVector3 EX { get; } = new FVector3(1, 0, 0);
        public static FVector3 EY { get; } = new FVector3(0, 1, 0);
        public static FVector3 EZ { get; } = new FVector3(0, 0, 1);
        public static FVector3 Ones { get; } = new FVector3(1, 1, 1);
        #endregion

        #region Properties
        public double X => _data[0];
        public double Y => _data[1];
        public double Z => _data[2];

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
            string z = Math.Round(_data[2], HelperFunctions.RoundDigits).ToString(formatting, formatProvider);
            
            return $"[{x},{y},{z}]";
        }
        #endregion

        #region IEquatable Members
        /// <summary>
        /// Equality overrides from <see cref="System.Object"/>
        /// </summary>
        /// <param name="obj">The object to compare this with</param>
        /// <returns>False if object is a different type, otherwise it calls <code>Equals(FVector3)</code></returns>
        public override bool Equals(object obj)
        {
            return obj is FVector3 vector&&Equals(vector);
        }

        public static bool operator ==(FVector3 target, FVector3 other) => target.Equals(other);
        public static bool operator !=(FVector3 target, FVector3 other) => !( target==other );


        /// <summary>
        /// Checks for equality among <see cref="FVector3"/> classes
        /// </summary>
        /// <param name="other">The other <see cref="FVector3"/> to compare it to</param>
        /// <returns>True if equal</returns>
        public bool Equals(FVector3 other)
        {
            return _data[0]==other._data[0]
                &&_data[1]==other._data[1]
                &&_data[2]==other._data[2];
        }

        /// <summary>
        /// Calculates the hash code for the <see cref="FVector3"/>
        /// </summary>
        /// <returns>The int hash value</returns>
        public override int GetHashCode()
        {
            unchecked
            {
                int hc = -1817952719;
                for (int i = 0; i<Size; i++)
                {
                    hc=( -1521134295 )*hc+_data[i].GetHashCode();
                }
                return hc;
            }
        }

        #endregion

        #region Algebra
        public static FVector3 Negate(FVector3 a) => FortranMethods.mul_scalar_vec3(-1.0, a);
        public static FVector3 Scale(double factor, FVector3 a) => FortranMethods.mul_scalar_vec3(factor, a);
        public static FVector3 Add(FVector3 a, FVector3 b) => FortranMethods.add_vec3_vec3(a, b);

        public static FVector3 Subtract(FVector3 a, FVector3 b) => FortranMethods.sub_vec3_vec3(a, b);
        public static double Dot(FVector3 A, FVector3 B) => FortranMethods.inner_vec3_vec3(A, B);
        public static FMatrix3 Outer(FVector3 a, FVector3 b) => FortranMethods.outer_vec3_vec3(a, b);
        public static double Cross(FVector3 a, FVector3 b) => FortranMethods.cross_vec3_vec3(a, b);
        #endregion

        #region Operators
        public static FVector3 operator +(FVector3 a, FVector3 b) => Add(a, b);
        public static FVector3 operator -(FVector3 a) => Negate(a);
        public static FVector3 operator -(FVector3 a, FVector3 b) => Subtract(a, b);
        public static FVector3 operator *(double f, FVector3 a) => Scale(f, a);
        public static FVector3 operator *(FVector3 a, double f) => Scale(f, a);
        public static FVector3 operator /(FVector3 a, double d) => Scale(1/d, a);
        public static double operator |(FVector3 a, FVector3 b) => Dot(a, b);
        #endregion

    }
}
