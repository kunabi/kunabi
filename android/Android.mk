# This is just an example showing the kind of code that 
# you will need when you want to incorporate a LispWorks 
# image into into an Android project in Eclipse/ADT
# that already uses JNI. 

# The OthelloDemo dones't use JNI, so doesn't actually use this.

# See the section "Delivering for Android" in the online
# manual for full explanation. 

# You will need to deliver your application passing
# :USING-JNI T, which will automatically put the LispWorks
# librray in the "jni" directory  inside the project directory. 
# Alternatively you can copy the LispWokrs library there by hand. 
# You then need to add the three lines at the bottom of this fil 
# to your own Android.mk to incorporate the LispWorks library 
# in the application.  Note that the LispWorks heap will still 
# need to be in the assets  directory. 

# Note that LispWorks is the default name, you can change that to
# match th elibrary name that you use when delivering. 

# When using Android Studio there is no need to anything special
# for JNI (checked on 0.4.6)

# Standard stuff
LOCAL_PATH := $(call my-dir)
include $(CLEAR_VARS)

# These are the three lines that you need to add to your
# Android.mk, potentially with different library name.  

LOCAL_SRC_FILES := libLispWorks.so
LOCAL_MODULE    := LispWorks
include $(PREBUILT_SHARED_LIBRARY)
