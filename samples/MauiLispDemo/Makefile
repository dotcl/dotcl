# MauiLispDemo task runner
#
# Usage:
#   make           # show help
#   make build-windows
#   make run-windows
#   make build-android
#   make run-android   # build APK + install + launch on connected device
#   make logcat        # stream device log filtered to MauiLispDemo
#   make scrcpy        # mirror device screen
#   make clean
#
# Environment-specific paths come from .env (gitignored). Copy .env.example
# to .env and adjust for your machine.

# Optional .env (per-machine config). `-include` does not error if missing.
-include .env
export

# --- Sensible defaults (override in .env) ---
JAVA_HOME             ?= C:\Program Files\Android\Android Studio\jbr
ANDROID_SDK_ROOT      ?= $(LOCALAPPDATA)\Android\Sdk
WINDOWS_TFM           ?= net10.0-windows10.0.19041.0
ANDROID_TFM           ?= net10.0-android
SCRCPY_FLAGS          ?= --max-size=1280 --max-fps=30 --video-bit-rate=4M --no-audio
LOGCAT_FILTER         ?= MauiLispDemo:V *:E

CSPROJ := MauiLispDemo.csproj

DOTNET_ANDROID_FLAGS := -p:UseAndroid=true -p:JavaSdkDirectory="$(JAVA_HOME)"

.DEFAULT_GOAL := help

.PHONY: help
help:
	@echo "MauiLispDemo task runner"
	@echo ""
	@echo "  build-windows    Windows MAUI build (default target)"
	@echo "  run-windows      dotnet run on Windows"
	@echo ""
	@echo "  restore-android  Force-restore with Android target enabled"
	@echo "  build-android    Android APK build"
	@echo "  run-android      Build + install + launch on connected device"
	@echo ""
	@echo "  logcat           Stream Android device logs for MauiLispDemo"
	@echo "  scrcpy           Mirror device screen ($(SCRCPY_FLAGS))"
	@echo "  devices          List connected adb devices"
	@echo ""
	@echo "  clean            rm -rf obj/ bin/"
	@echo ""
	@echo "Customize via .env (see .env.example)"

# --- Windows ---

.PHONY: build-windows
build-windows:
	dotnet build $(CSPROJ) -f $(WINDOWS_TFM)

.PHONY: run-windows
run-windows:
	dotnet run --project $(CSPROJ) -f $(WINDOWS_TFM)

# --- Android ---

.PHONY: restore-android
restore-android:
	rm -rf obj
	dotnet restore $(CSPROJ) -p:UseAndroid=true

.PHONY: build-android
build-android:
	dotnet build $(CSPROJ) $(DOTNET_ANDROID_FLAGS) -f $(ANDROID_TFM)

.PHONY: run-android
run-android:
	dotnet build $(CSPROJ) $(DOTNET_ANDROID_FLAGS) -f $(ANDROID_TFM) -t:Run

# --- Device tools ---

.PHONY: devices
devices:
	adb devices

.PHONY: logcat
logcat:
	adb logcat -s $(LOGCAT_FILTER)

.PHONY: scrcpy
scrcpy:
	# scrcpy は winget で Windows PATH に居るが MSYS2 PATH には通って
	# ない場合があるので、powershell 経由で起動する。MSYS2 側で scrcpy
	# がコマンドとして見えるなら直接呼んでも OK。
	powershell.exe -NoProfile -Command "scrcpy $(SCRCPY_FLAGS)"

# --- Cleanup ---

.PHONY: clean
clean:
	rm -rf obj bin
