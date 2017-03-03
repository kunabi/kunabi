/*
 ** $Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/android:KunabiDemo:src:com:lispworks:example:kunabidemo:History.java,v 1.3.1.1 2014/05/27 20:55:55 davef Exp $
 **
 ** Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.
 */

// A ListActivity that list the strings that were evaluated (by
// clicking "Evaluate teh string") in the lisp panel.
// Invoked by the "History" button.
// Gets an ArrayList<String> of items inside the intent as "extra" withname "HistroyItems"
// This is assumed to be called by StartActivityForresult.

package com.lispworks.example.kunabidemo;

import android.app.ListActivity;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;

public class History extends ListActivity {

	protected void onListItemClick(android.widget.ListView l,
			android.view.View v, int position, long id) {
		String item = (String) l.getItemAtPosition(position);

		// Setup the result for caller
		android.content.Intent in = new android.content.Intent();
		in.putExtra("result", item);
		setResult(RESULT_OK, in);

		finish();

	}

	/** Called when the activity is first created. */
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		// Add color to the divider, so can easily see where forms start and end
		android.graphics.drawable.ColorDrawable red = new android.graphics.drawable.ColorDrawable(
				android.graphics.Color.MAGENTA);
		getListView().setDivider(red);
		getListView().setDividerHeight(2);

		java.util.ArrayList<String> array = getIntent()
				.getStringArrayListExtra("HistroyItems");
		android.widget.ArrayAdapter<String> aa = new android.widget.ArrayAdapter<String>(
				getApplication(), android.R.layout.simple_list_item_1, array);
		setListAdapter(aa);
	}


  // Options meu, so users can go to Kunabi
	public boolean onCreateOptionsMenu(Menu menu) {
		 getMenuInflater().inflate(R.menu.others_menu, menu);
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		// Handle item selection
		switch (item.getItemId()) {
		case R.id.menu_main:
			android.content.Intent in = LispPanel.createIntent("Kunabi");
			startActivity( in);
			return true;
			default:			return super.onOptionsItemSelected(item);
		}
	}

}
