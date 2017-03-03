/*
 ** $Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/android:KunabiDemo:src:com:lispworks:example:kunabidemo:LispPanel.java,v 1.6.1.2 2014/11/19 16:03:20 martin Exp $
 **
 ** Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.
 */

package com.lispworks.example.kunabidemo;

import android.app.Activity;
import android.widget.TextView;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;

public class LispPanel extends Activity {
	TextView mTextView;
	TextView mInputView;
	static String mInputText = ""; // Keep the text in mInputView over Activity destroy/create

	static java.util.ArrayList<String> mHistory = new java.util.ArrayList<String>();
	static boolean mLoadedLibrary = false;
	// Some useful forms to test that LispWoks is working as expected
	// Called when the activity is first created.
	static {
		String[] historyInits = {
				"(mp:ps)",

				"(setq *computer-plays-waste-time-in-seconds* 2);; pretend that the computer takes time to compute its move",
				";; Define a function that can be used in the next too forms\n"
						+ "(defun eval-and-print (form)\n"
						+ "   (let ((res (eval form)))\n"
						+ "      (lw-ji:send-message-to-java-host (princ-to-string res) :reset)))",
				"(eval-and-print '(mp:get-current-process)); shows the Lisp process for the GUI thread",
				"(mp:funcall-async 'eval-and-print '(mp:get-current-process)); shows the background Exceute Lisp process",
				";; Create a process that execute events and print the results\n"
						+ "(progn\n"
						+ "  (defun loop-executing-events ()\n"
						+ "	  (loop (let ((event (mp:process-wait-for-event)))\n"
						+ "        (lw-ji:format-to-java-host \"~%got event ~s\" event)\n"
						+ "          (let ((res (mp:general-handle-event event)))\n"
						+ "             (lw-ji:format-to-java-host \"~%Handling got ~s\" res)))))\n"
						+ "  (setq loop-executing-events-process (mp:process-run-function \"Loop Execute Events\"\n"
						+ "                                                          () 'loop-executing-events))))",

				"(mp:process-send loop-executing-events-process '(mp:get-current-process)) ; Prints the process object of the event executing process",
				"(kunabi-user-change-a-square 5 2); change a square 5 in the Kunabi board to black",
				";; Start a background \"computation\" which reports its result continuously\n"
						+ "(mp:process-run-function \"multiplier\" () \n    #'(lambda() \n     (setq *finish-multiply* nil)\n    (dotimes (x 100)"
						+ "\n        (sleep 1)\n        (when *finish-multiply* (return))\n        (lw-ji:format-to-java-host \"~%~d * ~d = ~d\" x x (* x x))))))",
				"(setq *finish-multiply* t) ; stop the multiplying process",
				"(mp:process-run-function \"Error\" () #'(lambda () (open \"junk;;file::name\"))) ; cause an error on another process",
				"(raise-alert-dialog \"What do you want to eat?\"\n"
						+ "    :ok-title \"Chicken \" :ok-callback   '(raise-alert-dialog \"Here is some chicken\")\n"
						+ "     :cancel-title \"Salad \" :cancel-callback '(raise-alert-dialog \"We don't have salad\"))",
				"(raise-a-toast \"Bla Bla Bla\" :gravity :left)"

		};
		for (int ii = 0; ii < historyInits.length; ii++) {
			mHistory.add(historyInits[ii]);
		}
	}

	// Test if can evaluate stuff. Here we simply tests if the function
	// eval-for-android
	// (defined in the examples file andorid/android-kunabi-user.lisp) is
	// defined. It will
	// be undefined if the symbol is shakem.

	static boolean canEvaluate() {
		return
				(com.lispworks.Manager.status() == com.lispworks.Manager.STATUS_READY) &&
				com.lispworks.LispCalls.checkLispSymbol("EVAL-FOR-ANDROID", true);
	}

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		if (canEvaluate()) {
			// We can evaluate, put the lisp panel in.
			setContentView(R.layout.lisp_panel);
			TextView tv = (TextView) findViewById(R.id.main_text_view);
			mTextView = tv;

			TextView inputView = (TextView) findViewById(R.id.input_text_view);
			mInputView = inputView;
			inputView.setText(mInputText);
		} else {
			// No evaluation, only output
			mInputView = null;
			setContentView(R.layout.output_only);
			TextView tv = (TextView) findViewById(R.id.output_only_view);
			mTextView = tv;
		}
	}

	public void onStart() {
		super.onStart();
		// Make sure that output go to our mTextView
		com.lispworks.Manager.setTextView(mTextView);

		// disable buttons that don't work with Loaded LispWorks
		// if (! mLoadedLibrary) { enableButtons(false); }

		// Make sure error reports go to our mTextView
		com.lispworks.Manager.setErrorReporter(createReporter());
	}

	public void onStop() {
		com.lispworks.Manager.setTextView(null);
		if (mInputView != null)
			mInputText = mInputView.getText().toString();
		super.onStop();
	}

	public void onResume() {
		super.onResume();
		com.lispworks.Manager.setCurrentActivity(this); // allow raise-alert-dialog to work
	}

	public void onPause() {
		com.lispworks.Manager.setCurrentActivity(null); // make sure LispWorks don't try to use it
		super.onPause();
	}

	public void updateText(String newText) {
		mTextView.setText(newText);
	}

	public void appendText(String newText) {
		mTextView.append(newText);
	}

	// Some utility methods
	static android.content.Intent createIntent(String activityName) {
		android.content.Intent in = new android.content.Intent();
		String packageName = LispPanel.class.getPackage().getName();
		in.setClassName(packageName, packageName + "." + activityName);
		return in;
	}

	static android.content.Intent createIntentForHistory() {
		android.content.Intent in = createIntent("History");
		in.putStringArrayListExtra("HistroyItems", mHistory);
		return in;
	}

	// LispErrorReporter is an interface, we create here an anonymous class.
	// Since we use addMessage, we don't have to worry which process this is
	// called on.
	com.lispworks.Manager.LispErrorReporter createReporter() {
		return new com.lispworks.Manager.LispErrorReporter() {
			public boolean report(String errorString, String filename) {
				com.lispworks.Manager.addMessage(
						"---------Java reporting Lisp error:\n" + errorString
								+ "\n>> Bug for file:" + filename + "\n",
						com.lispworks.Manager.ADDMESSAGE_APPEND);
				return true; // Tell the caller we reported the error, so not
								// insert anything to the Textview.
			}
		};
	}

	/* buttons onClick methods */

	public void clearOutput(View view) {
		updateText("[cleared]");
	}

	public void showHistory(View view) {
		startActivityForResult(createIntentForHistory(), 1);
	}

	public void doEvaluation(View view) {
		internalDoEvaluation();

	}

	void internalDoEvaluation() {

		TextView inputTextView = (TextView) findViewById(R.id.input_text_view);
		String input = inputTextView.getText().toString();
		if (input.length() > 0) {
			Integer res = com.lispworks.LispCalls.callIntV("EVAL-FOR-ANDROID",
					input);
			mHistory.remove(input);
			mHistory.add(0, input);
			appendText("\n===================== Call eval returned ========> "
					+ res.toString());
		}

	}

	public void showBugFormLogs(View view) {
		com.lispworks.Manager.showBugFormLogs(this);
	}

	public void clearBugFormLogs(View view) {
		com.lispworks.Manager.clearBugFormLogs(0);
	}

	/* end of button onClick methods */

	static void setInputText(String str) {
		mInputText = str;
	}

	protected void onActivityResult(int requestCode, int resultCode,
			android.content.Intent data) {
		switch (requestCode) {
		case 1: /* showHistory */
			if (resultCode == RESULT_OK) {
				mInputText = data.getStringExtra("result");
				mInputView.setText(mInputText);
				break;
			}
		}
	}




	public boolean onCreateOptionsMenu(Menu menu) {
		 getMenuInflater().inflate(R.menu.others_menu, menu);
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		// Handle item selection
		switch (item.getItemId()) {
		case R.id.menu_main:
			android.content.Intent in = createIntent("Kunabi");
			startActivity( in);
			return true;
			default:			return super.onOptionsItemSelected(item);
		}
	}

}
