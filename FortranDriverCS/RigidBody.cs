// #define USE_CODE_GEN

using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text.Json;

using Microsoft.VisualBasic;

namespace FortranDriver
{
    public delegate void SumLoads(
        double t,
        double[] pos,
        NativeQuaternion ori,
        double[] vel,
        double[] omg,
        out double[] frc,
        out double[] tau);
    public readonly struct RigidBody
    {
        public readonly double mass;
        public readonly double Ixx, Iyy, Izz;

        public static readonly double[] o_ = [0.0, 0.0, 0.0];
        public static readonly double[] i_ = [1.0, 0.0, 0.0];
        public static readonly double[] j_ = [0.0, 1.0, 0.0];
        public static readonly double[] k_ = [0.0, 0.0, 1.0];
        public static readonly double[] q_eye = [1.0, 0.0, 0.0, 0.0];
        public static readonly double[] gee_ = [0.0, -10.0, 0.0];

        public RigidBody(double mass, double Ixx, double Iyy, double Izz)
        {
            this.mass=mass;
            this.Ixx=Ixx;
            this.Iyy=Iyy;
            this.Izz=Izz;
            this.Loads = SetGravityLoads;
        }
        public RigidBody(double mass, double Ixx, double Iyy, double Izz, SumLoads loads)
            : this(mass, Ixx, Iyy, Izz)
        {
            this.Loads = loads;
        }

        public void SetGravityLoads(double t,
                double[] pos,
                NativeQuaternion ori,
                double[] vel,
                double[] omg,
                [Out] out double[] frc,
                [Out] out double[] tau)
        {
            var R = ori.ToRotationMatrix();

            frc = new double[3];
            tau = new double[3];
            for (int i = 0; i < 3; i++)
            {
                frc[i] = mass * gee_[i];
                tau[i] = 0.0;
            }
        }

        public SumLoads Loads { get; }

        public void GetStateDerivative(double t, double[] y, out NativeVector yp)
        {
            SumLoads f = Loads;
            void nativeLoads(
                double t,
                double[] pos,
                double[] ori,
                double[] vel,
                double[] omg,
                [Out] out double[] frc,
                [Out] out double[] tau)
            {
                var q = new NativeQuaternion(ori);
                f(t, pos, q, vel, omg, out frc, out tau);
            };
            double[] data = new double[13];
            rb_state_derivative(this, t, y, nativeLoads, data);
            yp = new NativeVector(data);
        }

        public void GetState(double[] pos, NativeQuaternion ori, double[] vee, double[] omg, out NativeVector y)
        {
            var data = new double[13];
            rb_get_state(this, pos, ori.Data, vee, omg, data );
            y = new NativeVector(data);
        }
        public void SetState(NativeVector y, out double[] pos, out NativeQuaternion ori, out double[] vee, out double[] omg)
        {
            pos = new double[3];
            var q = new double[4];
            vee = new double[3];
            omg = new double[3];
            rb_set_state(this, y.Data, pos, q, vee, omg);
            ori = new NativeQuaternion(q);
        }

        [DllImport(
            FortranMethods.libraryName,
            CallingConvention = CallingConvention.Cdecl,
            CharSet = CharSet.Ansi,
            EntryPoint = "rb_get_state"
            )]
        static extern void rb_get_state(
            [In] RigidBody rb,
            double[] pos,
            double[] ori,
            double[] vee,
            double[] omg,
            [Out] double[] y);

        [DllImport(
            FortranMethods.libraryName,
            CallingConvention = CallingConvention.Cdecl,
            CharSet = CharSet.Ansi,
            EntryPoint = "rb_set_state"
            )]
        static extern void rb_set_state(
            [In] RigidBody rb,
            double[] y,
            [Out] double[] pos,
            [Out] double[] ori,
            [Out] double[] vee,
            [Out] double[] omg);

        delegate void SumLoadsNative(
        double t,
        double[] pos,
        double[] ori,
        double[] vel,
        double[] omg,
        [Out] out double[] frc,
        [Out] out double[] tau);
        [DllImport(
            FortranMethods.libraryName,
            CallingConvention = CallingConvention.Cdecl,
            CharSet = CharSet.Ansi,
            EntryPoint = "rb_state_derivative"
            )]
        static extern void rb_state_derivative(
            [In] RigidBody rb,
            double time,
            double[] y,
            [MarshalAs(UnmanagedType.FunctionPtr)] SumLoadsNative f,
            [Out] double[] yp);

    }
}
