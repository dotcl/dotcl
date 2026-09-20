# Android components written in Common Lisp

Android instantiates a service or an activity from the class name in
`AndroidManifest.xml`, on the Java side, with `Class.forName`. A component
written for .NET works because
the Android build scans the compiled IL, generates a Java shim class for each
managed type that derives from a Java type (an Android Callable Wrapper, or
ACW), and puts that shim in the APK's dex. The shim holds no logic: it binds to
the managed peer through `mono.android.Runtime.register` and forwards each
override over JNI.

Types that dotcl creates at run time with Reflection.Emit are invisible to that
build-time scan. But `dotnet:library` writes types to a real .dll on disk, and a
.dll *is* visible to the scan. So the component can be written in Lisp.

This sample emits a `[Service]` type and an `[Activity]` type from Lisp, feeds
the resulting .dll files to a minimal `net10.0-android` app, and shows the Java
wrappers and the dex entries that come out the other end.

`emit-service.lisp` is the smaller of the two and came first. `emit-activity.lisp`
adds the launcher activity, which needs named properties on the attribute:
`Android.App.ActivityAttribute` has a single parameterless constructor, so
`MainLauncher` (what makes the SDK write the MAIN/LAUNCHER intent-filter) and
`Label` are reachable only as `:main-launcher` / `:label`.

## What makes it work

The emitting process is a desktop (net10.0) dotcl, which has no Java runtime.
It still gets the real `Android.App.Service` and `Android.App.Activity` types,
because the Android *runtime* pack ships an implementation `Mono.Android.dll`:

```
<dotnet root>/packs/Microsoft.Android.Runtime.36.android/<version>/runtimes/android/lib/net10.0/Mono.Android.dll
```

Unlike the reference assembly under `Microsoft.Android.Ref.36`, that one carries
no `[ReferenceAssembly]` attribute, so CoreCLR loads it and answers `GetType` /
`GetConstructor` / `GetMethod` normally. No Mono.Android code is ever run in the
emitting process, so no Java runtime is needed.

Deriving from the real type also means the saved .dll's assembly reference
already reads `Mono.Android, Version=0.0.0.0, PublicKeyToken=84e04ff9cfb79065`.
That reference is the single thing the Android build looks for when it decides
whether an assembly may contribute Java types, so nothing has to be rewritten
afterwards.

Two members are load-bearing for the wrapper:

- the `(IntPtr, Android.Runtime.JniHandleOwnership)` constructor, chained to the
  base constructor (which is `protected` on both base types), plus a
  parameterless constructor. Java-side construction reaches managed code through
  `mono.android.TypeManager.Activate`, which needs them.
- for the service, an override of `OnBind`, which is abstract on
  `Android.App.Service` even for a service that is not bindable.
  `Android.App.Activity` has no abstract member, so the activity overrides only
  what it wants to act on.

Each override's JNI signature and connector name are derived by the generator
from the `[Register]` attribute on the *base* method, so the Lisp side declares
nothing about JNI.

## Running it

Emit the libraries (desktop dotcl). Either script can be run on its own; the
consumer project references whichever assemblies are present:

```sh
make cross-compile build
dotnet run -c Release --project runtime/runtime.csproj -- \
  --asm compiler/cil-out.sil samples/AndroidServiceLispSpike/emit-service.lisp
dotnet run -c Release --project runtime/runtime.csproj -- \
  --asm compiler/cil-out.sil samples/AndroidServiceLispSpike/emit-activity.lisp
```

Inspect what came out:

```sh
pwsh -NoProfile -File samples/AndroidServiceLispSpike/verify-emitted-dll.ps1
pwsh -NoProfile -File samples/AndroidServiceLispSpike/verify-emitted-dll.ps1 \
  -Dll samples/AndroidServiceLispSpike/out/LispAndroidActivitySpike.dll -Type LispSpike.LispMainActivity
```

Ask the Android SDK's own wrapper generator whether it accepts the type. This
drives the same library the `_GenerateJavaStubs` target uses, so it answers in a
second, with no Android project and no device:

```sh
pwsh -NoProfile -File samples/AndroidServiceLispSpike/run-acw-generator.ps1
pwsh -NoProfile -File samples/AndroidServiceLispSpike/run-acw-generator.ps1 \
  -Dll samples/AndroidServiceLispSpike/out/LispAndroidActivitySpike.dll
```

Build the consumer app (needs the `android` workload and a JDK):

```sh
cd samples/AndroidServiceLispSpike/consumer
dotnet build -c Debug -p:JavaSdkDirectory="<path to JDK>"
```

Then check the three places each type has to appear:

```sh
# the generated Java wrappers
cat obj/Debug/net10.0-android/android-arm64/android/src/crc64*/LispEchoService.java
cat obj/Debug/net10.0-android/android-arm64/android/src/crc64*/LispMainActivity.java
# the manifest entries the SDK wrote from [Service] and [Activity]
grep -E "service|activity" obj/Debug/net10.0-android/android-arm64/android/AndroidManifest.xml
# the classes in the dex
unzip -o bin/Debug/net10.0-android/android-arm64/*-Signed.apk classes.dex -d /tmp/apk
"$LOCALAPPDATA/Android/Sdk/build-tools/36.1.0/dexdump.exe" -f /tmp/apk/classes.dex \
  | grep -E "LispEchoService|LispMainActivity"
```

For the activity the manifest is the interesting half, because it is where the
named properties surface. Expect an entry carrying the label, and a nested
intent-filter that `MainLauncher` produced:

```xml
<activity android:label="Lisp Activity" android:name="crc64....LispMainActivity" android:exported="true">
  <intent-filter>
    <action android:name="android.intent.action.MAIN" />
    <category android:name="android.intent.category.LAUNCHER" />
  </intent-filter>
</activity>
```

An activity without that intent-filter means the attribute's named properties did
not reach the emitted metadata, which is the one thing this half of the sample is
there to prove.

## What to watch when the activity half is first built

Two things differ from the service and are worth checking before assuming a
failure is something deeper:

- **The overrides are protected on the base.** `Activity.OnCreate(Bundle)` and
  `OnResume` are `protected virtual`, where every Service override was public.
  They are emitted public, which widens accessibility rather than narrowing it,
  so the runtime should accept them. A `TypeLoadException` about method
  accessibility at emit time would point here.
- **`OnCreate` must call the base implementation.** Android raises
  `SuperNotCalledException` if an activity returns from `OnCreate` without it,
  so the Lisp body calls `dotnet:call-base` first. That path only runs on a
  device, so it is a stage-4 check, not a build-time one.

## Limits

- **Debug only.** `AndroidLinkMode=None` is set in the consumer project because
  nothing in it references the emitted type, so a trimming build would remove
  the type before the wrapper is generated. Reflection.Emit is also unavailable
  under Release AOT.
- **The Java class name is generated** unless the attribute names it. Without a
  name, the wrapper lands in a `crc64<hash>` package derived from the assembly
  name. `:name "com.example.Worker"` fixes it, the same way `:main-launcher` and
  `:label` are passed in the activity sample.
- **Running the overrides needs the Lisp loaded.** A saved library's method
  bodies dispatch by type name and signature into a table that the emitting
  process fills but the saved .dll does not carry. On the device the same Lisp
  definitions have to be loaded before the first callback arrives.
- **Debug builds do not embed assemblies in the APK.** They are deployed
  alongside it; `-p:EmbedAssembliesIntoApk=true` puts them inside.
