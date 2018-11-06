#include <jni.h>
#include <stdio.h>
#include "LinenJNI.h"

JNIEXPORT void JNICALL Java_LinenJNI_sayHello(JNIEnv *env, jobject thisObj) {
	printf("hello worldz!!!\n");
	return;
}

JNIEXPORT jstring JNICALL Java_LinenJNI_getStringMsg(JNIEnv *env, jobject thisObj) {
	return (**env).NewStringUTF(env, "This is a test");
}

