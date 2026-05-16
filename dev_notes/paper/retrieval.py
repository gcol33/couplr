import sys
from pathlib import Path

sys.path.insert(0, str(Path.home() / ".python"))
from retrieve_papers import retrieve_all

OUTPUT_DIR = Path(__file__).parent

retrieve_all(
    output_dir=OUTPUT_DIR,
    papers=[
        ("GabowTarjan_1989_FasterScalingAlgorithms.pdf", "10.1137/0218069"),
    ],
)
