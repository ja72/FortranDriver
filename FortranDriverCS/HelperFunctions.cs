using System.Diagnostics.Contracts;

namespace FortranDriver
{
    public static class HelperFunctions
    {
        public static T[] BuildArray<T>(int size, T defaultValue)
        {
            var result = new T[size];
            for (int i = 0; i < size; i++)
            {
                result[i] = defaultValue;
            }
            return result;
        }
        public static T[] BuildArray<T>(int size, Func<int, T> init)
        {
            Contract.Requires(init != null);
            var result = new T[size];
            for (int i = 0; i < size; i++)
            {
                result[i] = init(i+1);
            }
            return result;
        }
        public static T[,] BuildArray<T>(int rows, int columns, T defaultValue)
        {
            var result = new T[columns, rows];
            for (int i = 0; i < columns; i++)
            {
                for (int j = 0; j < rows; j++)
                {
                    result[i, j] =  defaultValue;
                }
            }
            return result;
        }
        public static T[,] BuildArray<T>(int rows, int columns, Func<int, int, T> init = null)
        {
            Contract.Requires(init != null);
            var result = new T[columns, rows];
            for (int i = 0; i < columns; i++)
            {
                for (int j = 0; j < rows; j++)
                {
                    result[i, j] = init(j+1, i+1);
                }
            }
            return result;
        }

        public static bool ValueEquals<T>(this T[] first, T[] second) where T : IEquatable<T>
            => Enumerable.SequenceEqual(first, second);

        public static void ShowInConsole(this double[] A, int width = 11, string formatting = "g4")
        {
            int rows = A.GetLength(0);
            for (int i = 0; i < rows; i++)
            {
                Console.Write("|");
                string text = A[i].ToString(formatting).PadLeft(width);
                if (text.Length>width)
                {
                    text = text.Substring(0, width-1) + "…";
                }
                Console.Write($" {text}");
                Console.WriteLine(" |");
            }
            Console.WriteLine();
        }

        public static void ShowInConsole(this double[,] A, int width = 11, string formatting = "g4")
        {
            int rows = A.GetLength(1), cols = A.GetLength(0);

            for (int i = 0; i < rows; i++)
            {
                Console.Write("|");
                for (int j = 0; j < cols; j++)
                {
                    string text = A[j, i].ToString(formatting).PadLeft(width);
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

    }
}
