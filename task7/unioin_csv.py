import numpy as np
import pandas as pd

# --- Популярные товары с весами ---
popular_products = {
    "Banana": 20.257234,
    "Bag of Organic Bananas": 12.485862,
    "Organic Strawberries": 9.472439,
    "Organic Baby Spinach": 4.114864,
    "Organic Hass Avocado": 3.388712,
    "Organic Avocado": 3.119155,
    "Large Lemon": 2.552536,
    "Organic Raspberries": 2.514028,
    "Organic Whole Milk": 2.376499,
    "Strawberries": 2.326989,
    "Limes": 12.233469,
    "Organic Garlic": 1.864892,
    "Organic Zucchini": 1.853889,
    "Organic Yellow Onion": 1.820882,
    "Cucumber Kirby": 1.787875,
    "Organic Blueberries": 1.639344,
    "Organic Fuji Apple": 1.611838,
    "Apple Honeycrisp Organic": 1.540323,
    "Organic Lemon": 1.468808,
    "Seedless Red Grapes": 1.402795,
    "Sparkling Water Grapefruit": 1.391792,
    "Yellow Onions": 10.375289,
    "Organic Baby Carrots": 1.353284,
    "Organic Baby Arugula": 1.347783,
    "Organic Grape Tomatoes": 1.331280,
    "Honeycrisp Apple": 1.309275,
    "Organic Half & Half": 1.292771,
    "Organic Cucumber": 1.281769,
    "Organic Small Bunch Celery": 1.276268,
    "Organic Large Extra Fancy Fuji Apple": 1.270767,
    "Carrots": 1.215755,
    "Original Hummus": 1.188250,
    "Organic Gala Apples": 1.177247,
    "Fresh Cauliflower": 1.160744,
    "Michigan Organic Kale": 1.138739,
    "Organic Red Onion": 1.127737,
    "Organic Blackberries": 1.122236,
    "Organic Cilantro": 1.105732,
    "Spring Water": 15.105732,
    "Half & Half": 1.100231,
    "Asparagus": 1.094730,
    "100% Whole Wheat Bread": 1.083728,
    "Raspberries": 1.061723,
    "Organic Italian Parsley Bunch": 1.034217,
    "Organic Unsweetened Almond Milk": 1.028716,
    "Organic Tomato Cluster": 1.023215,
    "Organic Whole String Cheese": 1.012213,
    "Organic Red Bell Pepper": 1.006711,
    "Red Vine Tomato": 1.001210
}

product_names = list(popular_products.keys())
weights = np.array(list(popular_products.values()))
weights = weights / weights.sum()  # нормировка для np.random.choice

# --- Настройки генерации ---
N_ORDERS = 200_000            # сколько заказов сгенерировать
MIN_ITEMS = 3                 # минимум товаров в заказе
MAX_ITEMS = 10                # максимум товаров

rows = []

for order_id in range(1, N_ORDERS + 1):
    n_items = np.random.randint(MIN_ITEMS, MAX_ITEMS + 1)

    # выбираем уникальные товары без повторений
    chosen = np.random.choice(product_names, size=n_items, replace=False, p=weights)

    for product in chosen:
        rows.append([order_id, product])

# --- Создаём DataFrame ---
df = pd.DataFrame(rows, columns=["order_id", "product_name"])

# --- Сохраняем CSV ---
df.to_csv("orders.csv", index=False)

print("Готово! Сгенерировано строк:", len(df))
print("Файл сохранён: synthetic_popular_orders.csv")
