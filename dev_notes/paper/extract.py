import fitz
import sys

doc = fitz.open("dev_notes/paper/GabowTarjan_1989_FasterScalingAlgorithms.pdf")
start = int(sys.argv[1])
end = int(sys.argv[2])
for i in range(start - 1, min(end, len(doc))):
    print(f"\n===== PAGE {i + 1} =====\n")
    print(doc[i].get_text())
