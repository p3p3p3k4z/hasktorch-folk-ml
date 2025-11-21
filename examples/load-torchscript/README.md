# Hybrid Haskell-Python Object Detector

This project successfully implements a **real-time image classification system** that bridges two distinct software ecosystems. It leverages the statistical power of Deep Learning with the safety and purity of Functional Programming.

## Project Architecture

The system is composed of two main parts:

1.  **"The Brain" (Haskell):** A command-line executable (`./load-torchscript`) that loads a pre-trained Convolutional Neural Network (CNN) model (`resnet_model.pt`) using `hasktorch`. Its job is to accept an image path and determine what object it contains.
2.  **"The Eyes" (Python):** An interface script (`webcam.py`) using `OpenCV` to capture webcam imagery. It takes a snapshot, sends it to the Haskell "brain," and displays the result to the user.

-----

## 🛠️ Installation and Execution Guide

Follow these steps to compile the Haskell environment, set up Python, and run the detector.

### 1\. Haskell Environment Compilation

Start from the top-level directory of the project (`hasktorch/`).

1.  **Configure the Cabal project:**

    ```bash
    $ ./setup-cabal.sh # Create a cabal project file
    ```

2.  **Build and test the core library:**

    ```bash
    $ cabal build hasktorch  # Build the Hasktorch library.
    $ cabal test hasktorch   # Build and run the Hasktorch library test suite.
    ```

3.  **Build the examples:**

    ```bash
    $ cabal build examples  # Build the Hasktorch examples.
    $ cabal test examples   # Build and run the Hasktorch example test suites.
    ```

> **NOTE:** While this process builds multiple examples, we will **ONLY** use the **`load-torchscript`** executable.

### 2\. Python Environment Setup

1.  **Create and activate venv:**

    ```bash
    python3 -m venv venv
    source venv/bin/activate
    ```

2.  **Install dependencies:**

    ```bash
    pip install torch torchvision
    pip install opencv-python
    ```

-----

### Generating the Pre-trained Model

Before running the detector, you must download and convert the ResNet-18 model into a format compatible with Haskell (TorchScript).

**Run the generation script:**

    ```bash
    python3 gen_resnet.py
    ```

## Running the Detector

You can run the system in three different modes.

### A. Manual Test (Direct Haskell)

Verify that the Haskell "brain" is working by classifying a static image directly from the CLI.

```bash
# Using cabal run (slower)
cabal run load-torchscript -- ./resnet_model.pt ./elephant.jpg

# Using the compiled executable (Recommended/Faster)
./load-torchscript ./resnet_model.pt ./chamaleon.jpeg
```

**Expected Output:**

```text
"--labels--"
["African_chameleon","tree_frog","green_lizard","common_iguana","leatherback_turtle"]
"--scores--"
Tensor Float [1,5] [[ 0.8308 , 2.2529e-2, 2.0475e-2, 1.8490e-2, 1.5495e-2]]
```

### B. CLI Wrapper Test

Use the Python wrapper to call the Haskell binary. This ensures environment variables (like `LD_LIBRARY_PATH`) are handled correctly.

```bash
python3 test_cli.py ./image
```

### C. Real-Time Webcam Detector

This is the main application. It opens the webcam, captures frames, and uses Haskell to classify them.

```bash
python3 webcam.py
```

  * **Usage:** Press `Space` to capture and analyze the current frame. Press `q` to quit.

-----

## Haskell and Functional Programming Analysis

The core logic resides in `Main.hs`, which serves as a perfect example of how functional code manages complex real-world tasks like AI inference.

### 1\. The Key Concept: The IO Monad

In Haskell, a "pure" function must always return the same output for the same input. However, reading arguments (`getArgs`) or loading files (`readImage...`) depends on the outside world. Haskell solves this with the **IO Monad**.

  * **`main :: IO ()`:** The main function returns an I/O "recipe"—a plan of action for the runtime—rather than a value.
  * **`do` notation and `<-`:** This syntax allows sequencing actions. For example, `[modelfile, inputfile] <- opt <$> getArgs` tells Haskell to execute the action `getArgs`, unwrap the result, and bind it to variables. This looks imperative but maintains functional purity.

### 2\. Pure Function Composition (The Pipeline)

The data processing pipeline demonstrates the power of composition:

```haskell
let img'' = toType Float $ hwc2chw $ normalize $ divScalar (255.0 :: Float) $ toType Float $ fromDynImage $ I.ImageRGB8 img'
```

[Image of data transformation pipeline]

Unlike imperative code that modifies variables step-by-step, Haskell uses the `$` operator to pass results from right to left:

1.  Take image `img'`.
2.  Wrap in `ImageRGB8`.
3.  Convert to Dynamic Image.
4.  Convert to Float type.
5.  Divide scalars (normalize 0-255).
6.  Normalize via standard deviation/mean.
7.  Permute dimensions (HWC to CHW).
8.  Final type conversion.

All these functions (`normalize`, `hwc2chw`) are **pure**: they transform data without side effects.

### 3\. Safety via Pattern Matching

Instead of `try-catch` blocks, Haskell uses types to enforce error handling:

```haskell
case mimg of
  Left err -> print err
  Right (img_, _) -> do ...
```

The compiler forces the programmer to handle both the Failure (`Left`) and Success (`Right`) cases, preventing unhandled runtime errors.

### 4\. Immutability

Data structures, such as the list of labels, are immutable:

```haskell
labels :: [String]
labels = [ "tench", "goldfish", ... ]
```

This definition is constant. The data cannot be modified in place; it exists as a definition that is accessed only when needed (Lazy Evaluation), saving memory and preventing state-related bugs.

-----

## 🔗 Complete System Architecture

1.  **Start (Python):** `webcam.py` initiates the camera loop.
2.  **Capture (Python):** User presses "Space." The script saves the frame to disk as `_frame_to_detect.jpg`. This file-based handoff solves timing issues associated with temporary files.
3.  **The Call (Python -\> Shell):** Python executes `subprocess.run(..., shell=True)`.
      * Using `shell=True` is the critical fix. It allows the subprocess to inherit the `LD_LIBRARY_PATH` from the shell, ensuring the executable can find the C++ `libtorch` libraries.
4.  **Execution (Haskell):** The shell runs `./load-torchscript ./resnet_model.pt ./_frame_to_detect.jpg`.
      * Haskell loads the model and image.
      * It runs the CNN pipeline.
      * It prints the result to `stdout`.
5.  **Response (Python):** `webcam.py` captures `stdout`, parses the first label (e.g., "African\_chameleon"), and draws it onto the video frame using OpenCV.