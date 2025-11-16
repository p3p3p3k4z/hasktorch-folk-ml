import subprocess
import os
import sys

# --- 1. CONFIGURACIÓN GLOBAL ---

# Rutas relativas a este script
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
HASKELL_EXE_PATH = "./load-torchscript"  # El ejecutable binario
MODEL_PATH = "./resnet_model.pt"       # El modelo que generaste

# --- 2. FUNCIÓN PRINCIPAL ---

def main():
    """
    Toma una imagen como argumento de la CLI y la pasa a Haskell.
    """
    
    # 1. Comprobar los argumentos de la CLI
    if len(sys.argv) != 2:
        print("¡Error! Debes proporcionar una ruta a la imagen.")
        print(f"Uso: python3 {sys.argv[0]} ./elephant.jpg")
        sys.exit(1)
        
    image_arg = sys.argv[1] # El segundo argumento (ej. ./elephant.jpg)
    model_arg = MODEL_PATH

    # 2. Comprobar que los archivos existen
    if not os.path.exists(os.path.join(SCRIPT_DIR, HASKELL_EXE_PATH)):
        print(f"Error: No se encuentra el ejecutable {HASKELL_EXE_PATH}")
        sys.exit(1)
    if not os.path.exists(os.path.join(SCRIPT_DIR, MODEL_PATH)):
        print(f"Error: No se encuentra el modelo {MODEL_PATH}")
        sys.exit(1)
    if not os.path.exists(image_arg):
        print(f"Error: No se encuentra la imagen {image_arg}")
        sys.exit(1)

    # 3. Construir el comando (¡idéntico a tu prueba manual!)
    command_string = f"{HASKELL_EXE_PATH} {model_arg} {image_arg}"
    
    print(f"Ejecutando comando en shell: {command_string}")

    try:
        # 4. ¡LA CLAVE! Ejecutar con shell=True
        # Esto replica tu terminal, usando el LD_LIBRARY_PATH correcto.
        result = subprocess.run(
            command_string,
            shell=True, # ¡Llamamos a la shell, como sugeriste!
            capture_output=True,
            text=True,
            timeout=10,
            cwd=SCRIPT_DIR, # Ejecutar desde la carpeta del script
            check=True      # Lanzará un error si Haskell falla
        )
        
        # 5. Si tiene éxito, imprimir la salida
        print("--- ¡Éxito! Salida de Haskell: ---")
        print(result.stdout.strip())

    except subprocess.CalledProcessError as e:
        # Si Haskell falla (returncode != 0), imprimir AMBAS salidas
        print("\n--- ¡ERROR! El comando de Haskell falló. ---")
        print("--- Salida Estándar (stdout): ---")
        print(e.stdout.strip())
        print("\n--- Salida de Error (stderr): ---")
        print(e.stderr.strip())
    except Exception as e:
        print(f"Error de Python: {e}")

# --- 3. PUNTO DE ENTRADA ---
if __name__ == "__main__":
    main()