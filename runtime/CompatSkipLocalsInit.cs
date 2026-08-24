#if NETSTANDARD2_0
using System;

// SkipLocalsInitAttribute is .NET 5+. The C# compiler honours one defined in the
// compiling assembly, so the emit-free target gets the same treatment.
//
// It lives in its own file because it has to be declared in the framework's own
// namespace: a block-scoped namespace cannot sit in a file that later opens a
// file-scoped one (CS8956), and Compat.cs is file-scoped.
namespace System.Runtime.CompilerServices
{
    [AttributeUsage(AttributeTargets.Module | AttributeTargets.Class | AttributeTargets.Struct
                    | AttributeTargets.Constructor | AttributeTargets.Method | AttributeTargets.Property
                    | AttributeTargets.Event | AttributeTargets.Interface, Inherited = false)]
    internal sealed class SkipLocalsInitAttribute : Attribute { }
}
#endif
