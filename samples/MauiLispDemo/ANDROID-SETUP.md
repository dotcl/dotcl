# Running MauiLispDemo on Android

The sample's primary build target is Windows. This note is for getting it
to run on a connected Android device.

## Prerequisites

- .NET 10 SDK
- `dotnet workload install maui-android`
- Android SDK Platform-Tools (for `adb`)
- Android SDK Platform `android-36` (the .NET 10 MAUI workload pins this)
- Android Studio JBR or any OpenJDK 17 — pointed at via `JAVA_HOME`
- A device with USB debugging on, or wireless ADB pairing (Android 11+)

Optional but useful:
- `scrcpy` — mirror the device screen to your desktop while you test

## Quick path

Copy `.env.example` to `.env` and adjust paths for your machine, then:

```sh
make build-android       # produces bin/Debug/net10.0-android/...-Signed.apk
adb install -r bin/Debug/net10.0-android/com.dotcl.mauilispdemo-Signed.apk
adb shell monkey -p com.dotcl.mauilispdemo -c android.intent.category.LAUNCHER 1
```

`make` targets:
- `build-android` / `run-android` — build and (with `-t:Run`) deploy
- `devices` / `logcat` / `scrcpy` — device-side helpers
- `restore-android` — force-restore once when first enabling Android
- `clean` — wipe `bin/` and `obj/`

## Notable build switches

The Makefile passes these so you don't have to remember:

- `-p:UseAndroid=true` — opt-in to dual-target (default Windows-only build
  doesn't require the maui-android workload)
- `-p:JavaSdkDirectory=...` — JDK path
- `-p:EmbedAssembliesIntoApk=true` — embed .NET assemblies in the APK so a
  plain `adb install` works. Without this the default Fast Deployment
  layout aborts at startup with `No assemblies found in
  /data/.../files/.__override__/arm64-v8a`.

## Troubleshooting

The MAUI Android pipeline is sensitive in a few specific ways. Quick
checks if something fails:

- **`No assemblies found in ...override...`** — missing
  `EmbedAssembliesIntoApk=true`.
- **`PlatformNotSupportedException` at startup** — outdated dotcl runtime,
  pre-D820. The Console.In/Out/Error guard there is required for
  no-console hosts (Android, services).
- **`BUILD-MAIN-PAGE has no function binding`** — `main.lisp` failed to
  load. Read `adb shell run-as com.dotcl.mauilispdemo cat
  files/dotcl-maui.log` for the boot trace.
- **`INSTALL_FAILED_USER_RESTRICTED`** on Xiaomi/MIUI — sideload
  restrictions. Try `adb shell pm install -i com.android.vending -r
  /path/to/apk.apk` to bypass the dialog.
- **USB transfers hanging or `remote decompress failed`** — switch to
  wireless ADB (`adb pair <ip:port> <code>`, then `adb connect <ip:port>`).
- **`apksigner verify` passes but device rejects with SHA-256 mismatch**
  — install via `pm install` instead of `adb install` (skips incremental
  install).

## Hardware notes

- **Windows ARM64 (Snapdragon) hosts**: Android Studio's emulator is not
  reliable on ARM64. Use a physical device. The host-side USB stack is
  also flaky for sustained transfers; wireless ADB is recommended.
- **scrcpy on ARM64 Windows**: lower decode/encoder performance than x64.
  Trim `--max-size` and `--max-fps` in `.env` for a smoother feed.
