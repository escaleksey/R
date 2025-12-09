import pandas as pd
from sklearn.feature_extraction.text import CountVectorizer

import spacy
import re
from nltk.corpus import wordnet
import nltk
nltk.download('wordnet')
nltk.download('omw-1.4')


nlp_ru = spacy.load("ru_core_news_sm")
nlp_en = spacy.load("en_core_web_sm")

df = pd.read_csv('task10.csv')
print(1)

stop_words = set(nltk.corpus.stopwords.words('russian')) | set(nltk.corpus.stopwords.words('english'))

def preprocess_text(text):
    # Очистка от лишних символов
    text = text.lower()
    text = re.sub(r'[^а-яА-Яa-zA-Z0-9\s]', ' ', text)

    tokens_ru = []
    tokens_en = []

    # spaCy обработка
    doc_ru = nlp_ru(text)
    doc_en = nlp_en(text)

    for token in doc_ru:
        if token.is_alpha and token.text not in stop_words:
            tokens_ru.append(token.lemma_)

    for token in doc_en:
        if token.is_alpha and token.text not in stop_words:
            tokens_en.append(token.lemma_)

    expanded_en = tokens_en.copy()

    expanded_ru = tokens_ru.copy()

    all_tokens = list(set(expanded_ru + expanded_en))
    return all_tokens

texts = df["question"]
texts = texts.apply(preprocess_text)
texts = texts.apply(lambda x: ' '.join(x))
vectorizer = CountVectorizer()

X = vectorizer.fit_transform(texts)
questions = df["question"]
answers = df["answer"]
print("Размерность матрицы Bag of Words:", X.shape)


bow_df = pd.DataFrame(X.toarray(), columns=vectorizer.get_feature_names_out())
print(bow_df.head())
lemma_counts = bow_df.sum(axis=0)

lemma_counts_df = pd.DataFrame(lemma_counts, columns=['count']).sort_values(by='count', ascending=False)

print(lemma_counts_df.head(20))

def jaccard_similarity_tokens(text1, text2):
    tokens1 = set(text1.lower().split())
    tokens2 = set(text2.lower().split())

    A = len(tokens1)
    B = len(tokens2)
    C = len(tokens1.intersection(tokens2))

    return C / (A + B - C) if (A + B - C) != 0 else 0

import numpy as np
from numpy.linalg import norm

def cosine_distance(v1, v2):
    v1 = v1.toarray()[0]
    v2 = v2.toarray()[0]

    dot = np.dot(v1, v2)
    normA = norm(v1)
    normB = norm(v2)

    if normA == 0 or normB == 0:
        return 1  # максимальная дистанция

    cosine_similarity = dot / (normA * normB)
    cosine_distance = 1 - cosine_similarity

    return cosine_distance

from scipy.stats import pearsonr
def correlation_similarity(v1, v2):
    v1 = v1.toarray()[0]
    v2 = v2.toarray()[0]
    if np.std(v1) == 0 or np.std(v2) == 0:
        return 0
    return pearsonr(v1, v2)[0]


def find_best_match(user_question):
    # Вектор для пользовательского вопроса
    user_question = " ".join(preprocess_text(user_question))
    user_vec = vectorizer.transform([user_question])

    best_scores = {
        "Косинус": {"score": 2, "index": -1},
        "Жаккар": {"score": -1, "index": -1},
        "Корреляция": {"score": -1, "index": -1},
        "Среднее": {"score": -1, "index": -1}
    }
    for i in range(len(questions)):
        q_vec = X[i]

        cos = cosine_distance(user_vec, q_vec)

        jac = jaccard_similarity_tokens(user_question, questions[i])

        corr = correlation_similarity(user_vec, q_vec)

        final_score = (cos + jac + corr) / 3

        if cos < best_scores["Косинус"]["score"]:
            best_scores["Косинус"]["score"] = cos
            best_scores["Косинус"]["index"] = i

        if jac > best_scores["Жаккар"]["score"]:
            best_scores["Жаккар"]["score"] = jac
            best_scores["Жаккар"]["index"] = i

        if corr > best_scores["Корреляция"]["score"]:
            best_scores["Корреляция"]["score"] = corr
            best_scores["Корреляция"]["index"] = i

        # Среднее
        avg_score = (-cos + jac + corr) / 3
        if avg_score > best_scores["Среднее"]["score"]:
            best_scores["Среднее"]["score"] = avg_score
            best_scores["Среднее"]["index"] = i

    # Формируем результаты
    results = {}
    for method, info in best_scores.items():
        idx = info["index"]
        results[method] = {
            "best_question": questions[idx],
            "best_answer": answers[idx],
            "score": info["score"]
        }

    return results


query = "Почем в Python  использовать виртуальные ?"
result = find_best_match(query)
print(result)

def testing(test_queries):
    for query in test_queries:
        result = find_best_match(query)
        print(f"ВОПРОС: {query}")
        print("-" * 50)
        for method, res in result.items():
            print(f"Метод: {method}")
            print(f"Лучший вопрос: {res['best_question']}")
            print(f"Ответ: {res['best_answer']}")
            print(f"Оценка сходства: {res['score']}")
            print("-" * 50)

import pandas as pd

def testing_table(test_queries):
    rows = []

    for query in test_queries:
        result = find_best_match(query)
        row = {"Query": query}

        for method, res in result.items():
            row[f"{method}_best_question"] = res['best_question']
            row[f"{method}_answer"] = res['best_answer']
            row[f"{method}_score"] = res['score']

        rows.append(row)

    df_results = pd.DataFrame(rows)
    return df_results

# Использование


import matplotlib.pyplot as plt
def plot_similarity_scores(df_results):
    methods = ['Косинус', 'Жаккар', 'Корреляция', 'Среднее']

    queries = df_results['Query']

    # Для каждого метода проверяем совпадение с query
    accuracy_matrix = {m: [] for m in methods}
    for i, query in enumerate(queries):
        for m in methods:
            best_question = df_results.loc[i, f"{m}_best_question"]
            # True если метод выбрал правильный вопрос
            accuracy_matrix[m].append(int(best_question == query))

    # График
    x = range(len(queries))
    width = 0.2

    plt.figure(figsize=(12, 6))
    for i, m in enumerate(methods):
        plt.bar([xi + i*width for xi in x], accuracy_matrix[m], width=width, label=m.capitalize())

    plt.xticks([xi + width*1.5 for xi in x], queries, rotation=30, ha='right')
    plt.ylabel("Верно предсказано (1 = Да, 0 = Нет)")
    plt.title("Сравнение методов по точности предсказания")
    plt.ylim(0, 1.2)
    plt.legend()
    plt.tight_layout()
    plt.show()

from sklearn.metrics import accuracy_score, precision_score, recall_score

def evaluate_methods(df_results):
    methods = ['Косинус', 'Жаккар', 'Корреляция', 'Среднее']
    queries = df_results['Query']

    metrics = {}

    for m in methods:
        y_true = [1]*len(queries)  # правильный ответ = 1 для всех
        y_pred = [1 if df_results.loc[i, f"{m}_best_question"] == queries[i] else 0 for i in range(len(queries))]

        acc = accuracy_score(y_true, y_pred)
        prec = precision_score(y_true, y_pred)
        rec = recall_score(y_true, y_pred)

        metrics[m] = {"Accuracy": acc, "Precision": prec, "Recall": rec}

    return metrics

# Использование





# Пример использования функции find_best_match
# 5 тестовых вопросов по работе с данными
test_queries = [
    "В чем различия между deepcopy и copy?",
    "Зачем в питоне нужны словари?",
    "Как работает распаковка значений?",
    "Зачем в Python  нужнен GIL?",
    "Зачем нужны lambda функции?"
]

testing(test_queries)





test_queries = [
    "В чем отличие deepcopy от copy в Python?",
    "Почему в Python важно использовать словари?",
    "Как в Python работает распаковка аргументов?",
    "Почему в Python существует глобальная блокировка интерпретатора (GIL)?",
    "Для чего используют функции lambda в Python?"
]
testing(test_queries)

from sklearn.feature_extraction.text import TfidfVectorizer
import pandas as pd

texts = df["question"]
texts = texts.apply(preprocess_text)
texts = texts.apply(lambda x: " ".join(x))
vectorizer = TfidfVectorizer(lowercase=True, stop_words='english')

X = vectorizer.fit_transform(texts)
questions = df["question"]
answers = df["answer"]
print("Размерность TF-IDF матрицы:", X.shape)

tfidf_df = pd.DataFrame(X.toarray(), columns=vectorizer.get_feature_names_out())
print(tfidf_df.head())

test_queries = [
    "В чем отличие deepcopy от copy в Python?",
    "Почему в Python важно использовать словари?",
    "Как в Python работает распаковка аргументов?",
    "Почему в Python существует глобальная блокировка интерпретатора (GIL)?",
    "Для чего используют функции lambda в Python?"
]
testing(test_queries)

print("=== Режим диалога с базой знаний Python ===")
print("Введите 'выход', чтобы завершить.")

while True:
    query = input("\nВаш вопрос: ")
    if query.lower() in ["выход", "exit", "quit"]:
        print("Диалог завершен.")
        break

    results = find_best_match(query)

    for method, res in results.items():
        print(f"\nМетод: {method}")
        print(f"Лучший вопрос: {res['best_question']}")
        print(f"Ответ: {res['best_answer']}")
        print(f"Оценка сходства: {res['score']:.3f}")