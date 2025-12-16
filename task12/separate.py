import os
import shutil
import random

# Путь к исходным данным
SOURCE_DIR = ""
TARGET_DIR = ""

CLASSES = ["rabbit", "not rabbit"]
SPLITS = {
    "train": 0.7,
    "test": 0.15,
    "validation": 0.15
}

IMAGE_EXTENSIONS = (".jpg", ".jpeg", ".png")

random.seed(42)

# Создание структуры папок
for split in SPLITS:
    for cls in CLASSES:
        os.makedirs(os.path.join(TARGET_DIR, split, cls), exist_ok=True)

for cls in CLASSES:
    cls_path = os.path.join(SOURCE_DIR, cls)
    images = [
        f for f in os.listdir(cls_path)
        if f.lower().endswith(IMAGE_EXTENSIONS)
    ]

    random.shuffle(images)

    total = len(images)
    train_end = int(total * SPLITS["train"])
    test_end = train_end + int(total * SPLITS["test"])

    split_files = {
        "train": images[:train_end],
        "test": images[train_end:test_end],
        "validation": images[test_end:]
    }

    for split, files in split_files.items():
        for fname in files:
            src = os.path.join(cls_path, fname)
            dst = os.path.join(TARGET_DIR, split, cls, fname)
            shutil.copy2(src, dst)

print("✅ Разделение завершено")
