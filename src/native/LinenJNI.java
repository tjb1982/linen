public class LinenJNI {
	static {
		System.loadLibrary("linenjni");
	}

	public native String getStringMsg();
}
