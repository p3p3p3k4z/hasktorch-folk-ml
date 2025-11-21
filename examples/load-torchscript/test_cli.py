import subprocess
import os
import sys

# --- 1. GLOBAL CONFIGURATION ---

# Paths relative to this script
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
HASKELL_EXE_PATH = "./load-torchscript"  # The compiled binary
MODEL_PATH = "./resnet_model.pt"       # The generated model

# --- 2. MAIN FUNCTION ---

def main():
    """
    Takes an image path as a CLI argument and passes it to the Haskell executable.
    Wrapper to handle environment variables correctly.
    """
    
    # 1. Check CLI arguments
    if len(sys.argv) != 2:
        print("Error! You must provide a path to the image.")
        print(f"Usage: python3 {sys.argv[0]} ./elephant.jpg")
        sys.exit(1)
        
    image_arg = sys.argv[1] # Second argument (e.g., ./elephant.jpg)
    model_arg = MODEL_PATH

    # 2. Check that files exist
    if not os.path.exists(os.path.join(SCRIPT_DIR, HASKELL_EXE_PATH)):
        print(f"Error: Could not find executable at {HASKELL_EXE_PATH}")
        print("Did you run 'cabal install ...' to copy the binary here?")
        sys.exit(1)
    if not os.path.exists(os.path.join(SCRIPT_DIR, MODEL_PATH)):
        print(f"Error: Could not find model at {MODEL_PATH}")
        sys.exit(1)
    if not os.path.exists(image_arg):
        print(f"Error: Could not find image at {image_arg}")
        sys.exit(1)

    # 3. Build the command string
    command_string = f"{HASKELL_EXE_PATH} {model_arg} {image_arg}"
    
    print(f"Running shell command: {command_string}")

    try:
        # 4. KEY STEP: Run with shell=True
        # This allows the subprocess to inherit the correct shell environment 
        # (specifically LD_LIBRARY_PATH for libtorch), preventing linking errors.
        result = subprocess.run(
            command_string,
            shell=True, 
            capture_output=True,
            text=True,
            timeout=10,
            cwd=SCRIPT_DIR, # Run from the script's directory
            check=True      # Raises CalledProcessError on failure
        )
        
        # 5. If successful, print output
        print("--- Success! Haskell Output: ---")
        print(result.stdout.strip())

    except subprocess.CalledProcessError as e:
        # If Haskell fails (returncode != 0), print both stdout and stderr
        print("\n--- ERROR! The Haskell command failed. ---")
        print("--- Standard Output (stdout): ---")
        print(e.stdout.strip())
        print("\n--- Standard Error (stderr): ---")
        print(e.stderr.strip())
    except Exception as e:
        print(f"Python execution error: {e}")

# --- 3. ENTRY POINT ---
if __name__ == "__main__":
    main()