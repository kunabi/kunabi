/*
** $Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/android:KunabiDemo:src:com:lispworks:example:kunabidemo:MyApplication.java,v 1.3.1.1 2014/05/27 20:55:55 davef Exp $
**
** Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.
*/

// An example of how to init LispWorks when the application starts, before
// anything is displayed.
// Not used in the demo.
// To use it, you need to add android:name="com.lispworks.example.kunabidemo.MyApplication"
// in the attributes of <application> in AndroidManifest.xml
// If you already have your own application sub-class, you can just add
// the com.lispworks.LispWorksNativeLibrary.init call to its onCreate.


package com.lispworks.example.kunabidemo;

import android.app.Application;

public class MyApplication extends Application {
	public void onCreate()
	{
		super.onCreate();
		com.lispworks.Manager.init(this);
	}
}
