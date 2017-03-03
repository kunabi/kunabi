;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/android:README.txt,v 1.15.1.3 2015/04/14 14:05:59 martin Exp $" -*-

;; Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.

This directory contains a demonstration of using a LispWorks delivered
application inside Android. See "The Othello Demo" for full discussion. 

To use it, you need to:
1) Create the Android project.
2) Deliver the LispWorks application.

Once you have done these steps, you can build the Android project
and deliver it as usual. When the application starts, you
can play Othello against it, and if you delivered "with lisp"
you can go to the Lisp Panel and evaluate Lisp forms.

===================================
1. Creating the Android project
===================================

The Android project code is in ./OthelloDemo/, which is the directory 
examples/android/OthelloDemo inside the the LispWorks distribution. You 
need to make a project with this code.

-----------------------------------------------
Eclipse with ADT (checked on ADT build: v22.6.2)
------------------------

a) You need to have a workspace, which maybe completely empty.
b) Choose  "File" -> "Import" from the menubar to raise the "Import" dialog.
c) In the "Import" dialog expand "Android", select 
   "Existing Android Code Into Workspace" and click "Next". 
d) You should be in the "Import Projects" tab. Click "Browse...".
e) Browse to the examples/android/OthelloDemo directory inside the
   LispWorks distribution directory, select it and press "Ok".
f) Click "Select All" to ensure that the  project is selected.
g) Tick on the "Copy projects into workspace"  check button (below the list).
h) Click "Finish".

The new project name defaults to "LispWorksRuntimeDemo", and this,
relative to the workspace, is the "project path" that you will need to
set the *project-path* variable to (below). You can change that before
clicking finish.

Note: if you get errors when trying to build, try:
   1) "Project" -> "Clean". 
   2) Check that the  Project build target" is 3.0 or higher. do:
       "Project" -> "Properties", click "Android", and it shows the list of targets
       available. Select the highest. 

-----------------------------------------------
Android studio (checked for 0.4.6 and 0.8.6)
--------------------------

1) Inside Android studio, Select "Import project .." either from the 
   "Welcome to Android studio" dialog or from the "File" menu. That
    raises "Select Gradle Project Import" dialog
2) In "Select Gradle Project Import" dialog, select the examples/android/OthelloDemo
   directory in the LispWorks distribution and press Ok. That should raise
   a dialog called "Import Project from ADT (Eclipse Android)". 
3) Select a directory to put the project in and press Ok. This is the "project
   path" that you will need  set the *project-path* variable to (below).
4) Leave all the default settings in the next page and press Finish. 



=====================================
2. Delivering the LispWorks application
=====================================
Copy one of the files deliver-android-othello.lisp or 
deliver-android-othello-with-lisp.lisp from the current directory.
Edit the *project-path* variable to point to the path of the 
Android project that you created above. For example:

(defvar *project-path* "~/my-workspace/LispWorksRuntimeDemo/")

Then use the copied file as the build script with the shell script
run-lw-android.sh from the current directory:

run-lw-android.sh -build <copy-of-file>


See the documentation for deliver-to-android-project for details of
delivering for Android.
