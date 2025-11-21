import cv2
import subprocess
import os
import time

# --- 1. GLOBAL CONFIGURATION ---

# Paths relative to this script
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
# Path to the wrapper script that handles environment variables correctly
TEST_CLI_SCRIPT = os.path.join(SCRIPT_DIR, "test_cli.py") 
CAPTURE_DIR = os.path.join(SCRIPT_DIR, "webcam_captures")
CAPTURE_FILE = os.path.join(CAPTURE_DIR, "_frame_to_detect.jpg")

# --- 2. SETUP FUNCTIONS ---

def check_files():
    """Checks if the required wrapper script exists."""
    print("--- Haskell + Python Object Detector ---")
    
    if not os.path.exists(TEST_CLI_SCRIPT):
        print(f"Error: Could not find the test script at ({TEST_CLI_SCRIPT})")
        return False
        
    print(f"  Inference Script: {TEST_CLI_SCRIPT}")
    print(f"  Capture Dir:      {CAPTURE_DIR}")
    return True

# --- 3. DETECTION FUNCTIONS ---

def parse_cli_output(output_str):
    """
    Parses the full text output from 'test_cli.py' 
    and returns the FIRST prediction label.
    """
    try:
        # Look for the Haskell output marker within the CLI output
        if "--labels--" not in output_str:
            print(f"Unexpected output from test_cli.py (stdout): {output_str}")
            return "Error: '--labels--' not found"
            
        # Isolate the Haskell section
        haskell_output = output_str.split("--- Success! Haskell Output: ---")[1]
        
        lines = haskell_output.strip().splitlines()
        # The line after "--labels--" contains the list
        labels_line = lines[1] 
        
        # Extract the first label (highest probability)
        # Ex: '["African_elephant","tusker",...]' -> "African_elephant"
        first_label = labels_line.split('"')[1] 
        
        return first_label

    except Exception as e:
        print(f"Error parsing output: {e}")
        return "Error: Parse failed"

def detect_animal_in_image(image_path):
    """
    Calls 'test_cli.py' with the captured image path.
    """
    
    # Ensure absolute path for safety
    image_arg = os.path.abspath(image_path)

    # Construct the command to call the Python wrapper
    command_string = f"python3 {TEST_CLI_SCRIPT} {image_arg}"
    
    try:
        # We use shell=True to ensure the python environment is inherited correctly
        result = subprocess.run(
            command_string,
            shell=True,
            capture_output=True,
            text=True,
            timeout=10, # 10s is sufficient for the full pipeline
            cwd=SCRIPT_DIR 
        )
        
        output = result.stdout.strip()
        
        if result.returncode != 0:
            print(f"test_cli.py failed (stderr): {result.stderr.strip()}")
            return "Inference Error"
            
        return parse_cli_output(output)

    except subprocess.TimeoutExpired:
        print("Error: Inference timed out ( > 10 seconds)!")
        return "Error: Timeout"
    except Exception as e:
        print(f"Detection error: {e}")
        return f"Error: {e}"

# --- 4. MAIN LOOP ---

def main_loop():
    """
    Opens the webcam and runs the detection loop.
    """
    cap = cv2.VideoCapture(0)
    if not cap.isOpened():
        print("Error: Could not open webcam.")
        return
        
    print("Press 'q' to quit, 'space' to analyze the current frame.")
    status = "Press 'space' to analyze"
    color = (255, 255, 255) # White

    while True:
        ret, frame = cap.read()
        if not ret: 
            break
        
        display_frame = frame.copy()
        key = cv2.waitKey(1) & 0xFF

        if key == ord('q'):
            break
            
        if key == ord(' '): # Trigger analysis
            print("Analyzing frame...")
            status = "Analyzing..."
            color = (255, 255, 0) # Yellow
            
            # Update UI immediately to show status
            cv2.putText(display_frame, status, (10, 30), 
                        cv2.FONT_HERSHEY_SIMPLEX, 1, color, 2)
            cv2.imshow('Haskell + Python Object Detector', display_frame)
            cv2.waitKey(1) # Force GUI update
            
            # Save capture to disk (solves timing issues)
            cv2.imwrite(CAPTURE_FILE, frame)
            
            # Call Haskell (via the wrapper script)
            prediction = detect_animal_in_image(CAPTURE_FILE)
            
            # Update status with result
            status = f"DETECTED: {prediction}"
            is_error = "Error" in prediction
            color = (0, 0, 255) if is_error else (0, 255, 0) # Red if error, Green if success
            print(f"Result: {status}")
        
        # Draw status on frame
        cv2.putText(display_frame, status, (10, 30), 
                    cv2.FONT_HERSHEY_SIMPLEX, 1, color, 2)
        cv2.imshow('Haskell + Python Object Detector', display_frame)
        
    cap.release()
    cv2.destroyAllWindows()
    print("Closing program.")
    
    # Cleanup
    if os.path.exists(CAPTURE_FILE):
        os.remove(CAPTURE_FILE)

# --- 5. ENTRY POINT ---
if __name__ == "__main__":
    
    if check_files():
        os.makedirs(CAPTURE_DIR, exist_ok=True)
        main_loop()
    else:
        print("Missing required files. Cannot start.")