import pandas as pd
import numpy as np
import random

popular_products = [
    "Banana", "Soap", "Strawberries", "Greek Yogurt", "Whole Milk",
    "Avocado", "Lemon", "Raspberries", "Blueberries", "Fuji Apple",
    "Honeycrisp Apple", "Garlic", "Zucchini", "Yellow Onion",
    "Cucumber", "Baby Spinach", "Baby Carrots", "Grape Tomatoes",
    "Carrots", "Hummus", "Gala Apples", "Cauliflower", "Kale",
    "Red Onion", "Blackberries", "Cilantro", "Still Water",
    "Sparkling Water", "Asparagus", "Whole Wheat Bread",
    "Italian Parsley", "Almond Milk", "Tomato Cluster",
    "String Cheese", "Red Bell Pepper", "Vine Tomato",
    "Salmon Fillet", "Pasta Penne", "Mozzarella Cheese",
    "Fresh Basil", "Olive Oil"
]

clusters = {
    "fruits": [
        "Banana", "Strawberries", "Blueberries", "Raspberries",
        "Fuji Apple", "Honeycrisp Apple", "Blackberries"
    ],
    "vegetables": [
        "Cucumber", "Zucchini", "Cauliflower", "Baby Carrots",
        "Red Onion", "Yellow Onion", "Red Bell Pepper",
        "Vine Tomato", "Tomato Cluster"
    ],
    "greens": [
        "Baby Spinach", "Kale", "Cilantro", "Italian Parsley", "Fresh Basil"
    ],
    "dairy_bakery": [
        "Greek Yogurt", "Whole Milk", "String Cheese",
        "Mozzarella Cheese", "Whole Wheat Bread"
    ],
    "water_fruit": [
        "Still Water", "Sparkling Water", "Banana", "Strawberries", "Blueberries"
    ],
    "fish_lemon": [
        "Salmon Fillet", "Lemon", "Olive Oil"
    ],
    "pasta_set": [
        "Pasta Penne", "Olive Oil", "Tomato Cluster",
        "Mozzarella Cheese", "Fresh Basil"
    ],
    "snack_combo": [
        "Hummus", "Carrots", "Baby Carrots", "Cucumber"
    ]
}

# веса для вероятностей
weights = {p: random.random() for p in popular_products}
weights = {k: v / sum(weights.values()) for k, v in weights.items()}

def generate_basket():
    basket_size = np.random.randint(2, 10)

    # выбираем тематический кластер 70% времени
    if random.random() < 0.7:
        cluster_key = random.choice(list(clusters.keys()))
        cluster_items = clusters[cluster_key]
    else:
        cluster_items = popular_products

    basket = []
    for _ in range(basket_size):
        # 70% — из кластера, 30% — из общего пула
        if random.random() < 0.7:
            item = random.choice(cluster_items)
        else:
            item = random.choices(popular_products, weights=list(weights.values()))[0]

        basket.append(item)

    basket = list(set(basket))

    return ", ".join(basket)


N = 150000

df = pd.DataFrame({
    "product_name": [generate_basket() for _ in range(N)]
})

df.to_csv("orders.csv", index=False)

print(df.head())
print("Generated:", len(df), "records")
