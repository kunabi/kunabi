/*
 ** $Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/android:AKunabiDemo:src:com:lispworks:example:kunabidemo:SquareLayout.java,v 1.1.1.1 2014/05/27 20:55:55 davef Exp $
 **
 ** Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.
 */
package com.lispworks.example.kunabidemo;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.FrameLayout;

// A layout that force itself to be square, by setting both width and height
// to the smallest of its given width and height.

public class SquareLayout extends FrameLayout {

	public SquareLayout (Context context) {
		super(context);

	}
	public SquareLayout (Context context, AttributeSet attrs) {
		super(context,attrs);

	}
	public SquareLayout (Context context, AttributeSet attrs, int defStyle) {
		super(context,attrs, defStyle);

	}
	public void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {

		int width = MeasureSpec.getSize(widthMeasureSpec);

		int height = MeasureSpec.getSize(heightMeasureSpec);
		if (width > height) width = height ;
		int actualValue = MeasureSpec.makeMeasureSpec(width, MeasureSpec.EXACTLY);

		super.onMeasure(actualValue,  actualValue) ;

	}


}
