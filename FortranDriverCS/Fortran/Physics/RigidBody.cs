// #define USE_CODE_GEN

using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using JA.Fortran.Arrays;

namespace JA.Fortran.Physics
{
    public delegate void rb_sum_loads(
            double t,
            FVector3 pos,
            FQuat4 ori,
            FVector3 vel,
            FVector3 omg,
            out FVector3 frc,
            out FVector3 tau);
    public readonly struct RigidBody
    {
        public readonly double mass;
        public readonly double Ixx, Iyy, Izz;

        public static readonly FVector3 o_ = new FVector3(0.0, 0.0, 0.0);
        public static readonly FVector3 i_ = new FVector3(1.0, 0.0, 0.0);
        public static readonly FVector3 j_ = new FVector3(0.0, 1.0, 0.0);
        public static readonly FVector3 k_ = new FVector3(0.0, 0.0, 1.0);
        public static readonly FQuat4 q_eye = new FQuat4(1.0, 0.0, 0.0, 0.0);
        public static readonly FVector3 gee_ = new FVector3(0.0, -10.0, 0.0);

        public RigidBody(double mass, double Ixx, double Iyy, double Izz)
        {
            this.mass = mass;
            this.Ixx = Ixx;
            this.Iyy = Iyy;
            this.Izz = Izz;
            Loads = SetGravityLoads;
        }
        public RigidBody(double mass, double Ixx, double Iyy, double Izz, rb_sum_loads loads)
            : this(mass, Ixx, Iyy, Izz)
        {
            Loads = loads;
        }

        public void SetGravityLoads(double t,
                FVector3 pos,
                FQuat4 ori,
                FVector3 vel,
                FVector3 omg,
                [Out] out FVector3 frc,
                [Out] out FVector3 tau)
        {
            //var R = ori.ToRotationMatrix();

            frc = mass * gee_;
            tau = FVector3.Zero;
        }

        public rb_sum_loads Loads { get; }

        public void GetStateDerivative(double t, FVector y, ref FVector yp)
        {
            //rb_sum_loads f = Loads;
            double[] data = yp.Data;
            rb_state_derivative(this, t, y, Loads, ref data);
            //yp = new FVector(data);
        }

        public void GetState(FVector3 pos, FQuat4 ori, FVector3 vee, FVector3 omg, ref FVector y)
        {
            double[] data = y.Data;
            rb_get_state(this, pos, ori, vee, omg, ref data);
            //y = new FVector(data);
        }
        public void SetState(FVector y, out FVector3 pos, out FQuat4 ori, out FVector3 vee, out FVector3 omg)
        {
            double[] data = y.Data;
            rb_set_state(this, data, out pos, out ori, out vee, out omg);
        }

        #region Fortran API
        const string libraryName = FortranMethods.libraryName;

        [DllImport(libraryName,
            EntryPoint = "rb_get_state",
            CharSet = CharSet.Ansi,
            CallingConvention = CallingConvention.Cdecl)]
        static extern void rb_get_state(
            [In] RigidBody rb,
            FVector3 pos,
            FQuat4 ori,
            FVector3 vee,
            FVector3 omg,
            [In, Out] ref double[] y);

        [DllImport(libraryName,
            EntryPoint = "rb_set_state",
            CharSet = CharSet.Ansi,
            CallingConvention = CallingConvention.Cdecl)]
        static extern void rb_set_state(
            [In] RigidBody rb,
            [In] double[] y,
            [Out] out FVector3 pos,
            [Out] out FQuat4 ori,
            [Out] out FVector3 vee,
            [Out] out FVector3 omg);

        [DllImport(libraryName,
            EntryPoint = "rb_state_derivative",
            CharSet = CharSet.Ansi,
            CallingConvention = CallingConvention.Cdecl)]
        static extern void rb_state_derivative(
            [In] RigidBody rb,
            double time,
            [In] double[] y,
            [MarshalAs(UnmanagedType.FunctionPtr)] rb_sum_loads f,
            [In, Out] ref double[] yp);

        [DllImport(libraryName,
            EntryPoint = "rb_get_inertia_matrix",
            CharSet = CharSet.Ansi,
            CallingConvention = CallingConvention.Cdecl)]
        static extern void rb_get_inertia_matrix(
            [In] RigidBody rb,
            [In] FVector3 pos,
            [In] FQuat4 ori,
            bool inverse = false);

        #endregion

    }


}
