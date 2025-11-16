import cv2
import subprocess
import os
import time

# --- 1. CONFIGURACIÓN GLOBAL ---

# Rutas relativas a este script
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
# ¡LA CLAVE! La ruta al script que SÍ funciona.
TEST_CLI_SCRIPT = os.path.join(SCRIPT_DIR, "test_cli.py") 
CAPTURE_DIR = os.path.join(SCRIPT_DIR, "webcam_captures")
CAPTURE_FILE = os.path.join(CAPTURE_DIR, "_frame_to_detect.jpg")

# --- 2. FUNCIONES DE CONFIGURACIÓN ---

def check_files():
    """Comprueba que el script test_cli.py exista."""
    print("--- Detector de Animales (Haskell + Python) ---")
    
    if not os.path.exists(TEST_CLI_SCRIPT):
        print(f"Error: No se encuentra el script de prueba en ({TEST_CLI_SCRIPT})")
        return False
        
    print(f"  Script de Inferencia: {TEST_CLI_SCRIPT}")
    print(f"  Capturas:   {CAPTURE_DIR}")
    return True

# --- 3. FUNCIONES DE DETECCIÓN ---

def parse_cli_output(output_str):
    """
    Parsea la salida de texto COMPLETA de 'test_cli.py'
    y devuelve la PRIMERA predicción.
    """
    try:
        # Buscamos la salida de Haskell dentro de la salida de test_cli.py
        if "--labels--" not in output_str:
            print(f"Salida inesperada de test_cli.py (stdout): {output_str}")
            return "Error: No se encontró '--labels--'"
            
        # Aislamos la parte de Haskell
        haskell_output = output_str.split("--- ¡Éxito! Salida de Haskell: ---")[1]
        
        lines = haskell_output.strip().splitlines()
        labels_line = lines[1] # La línea es "--labels--" y luego la lista
        
        # Extrae la primera etiqueta
        first_label = labels_line.split('"')[1] 
        
        return first_label

    except Exception as e:
        print(f"Error parseando la salida: {e}")
        return "Error: Parseo fallido"

def detect_animal_in_image(image_path):
    """
    Llama a 'test_cli.py' con la imagen capturada.
    """
    
    # Argumento de imagen (ruta absoluta para ser seguros)
    image_arg = os.path.abspath(image_path)

    # --- ¡LA SOLUCIÓN! ---
    # Llamamos al script que SÍ funciona
    command_string = f"python3 {TEST_CLI_SCRIPT} {image_arg}"
    
    try:
        # Usamos shell=True por si acaso el venv de python no está
        # en el path principal, pero 'python3' debería ser suficiente.
        result = subprocess.run(
            command_string,
            shell=True,
            capture_output=True,
            text=True,
            timeout=10, # 10s es suficiente para Python+Haskell
            cwd=SCRIPT_DIR 
        )
        
        output = result.stdout.strip()
        
        if result.returncode != 0:
            print(f"Error de test_cli.py (stderr): {result.stderr.strip()}")
            return "Error en test_cli.py"
            
        return parse_cli_output(output)

    except subprocess.TimeoutExpired:
        print("Error: ¡La ejecución de test_cli.py tardó más de 10 segundos!")
        return "Error: Timeout"
    except Exception as e:
        print(f"Error en detección: {e}")
        return f"Error: {e}"

# --- 4. BUCLE PRINCIPAL (Sin cambios) ---

def main_loop():
    """
    Abre la webcam y ejecuta el bucle de detección.
    """
    cap = cv2.VideoCapture(0)
    if not cap.isOpened():
        print("Error: No se pudo abrir la cámara")
        return
        
    print("Presiona 'q' para salir, 'espacio' para analizar el fotograma")
    status = "Presiona 'espacio'"
    color = (255, 255, 255) # Blanco

    while True:
        ret, frame = cap.read()
        if not ret: 
            break
        
        display_frame = frame.copy()
        key = cv2.waitKey(1) & 0xFF

        if key == ord('q'):
            break
            
        if key == ord(' '): # Analizar con barra espaciadora
            print("¡Analizando fotograma!")
            status = "Analizando..."
            color = (255, 255, 0) # Amarillo
            
            cv2.putText(display_frame, status, (10, 30), 
                        cv2.FONT_HERSHEY_SIMPLEX, 1, color, 2)
            cv2.imshow('Detector de Animales (Haskell + Python)', display_frame)
            cv2.waitKey(1) 
            
            # Guarda la captura en el archivo fijo
            cv2.imwrite(CAPTURE_FILE, frame)
            
            # ¡Llama al detector de Haskell (vía test_cli.py)!
            prediction = detect_animal_in_image(CAPTURE_FILE)
            
            # Actualiza el estado
            status = f"DETECTADO: {prediction}"
            is_error = "Error" in prediction
            color = (0, 0, 255) if is_error else (0, 255, 0)
            print(f"Resultado: {status}")
        
        cv2.putText(display_frame, status, (10, 30), 
                    cv2.FONT_HERSHEY_SIMPLEX, 1, color, 2)
        cv2.imshow('Detector de Animales (Haskell + Python)', display_frame)
        
    cap.release()
    cv2.destroyAllWindows()
    print("Cerrando programa.")
    if os.path.exists(CAPTURE_FILE):
        os.remove(CAPTURE_FILE)

# --- 5. PUNTO DE ENTRADA ---
if __name__ == "__main__":
    
    if check_files():
        os.makedirs(CAPTURE_DIR, exist_ok=True)
        main_loop()
    else:
        print("Faltan archivos esenciales. El programa no puede continuar.")