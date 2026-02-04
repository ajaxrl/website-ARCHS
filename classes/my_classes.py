import pandas as pd
import numpy as np
from gensim.models.doc2vec import Doc2Vec, TaggedDocument
from nltk.tokenize import word_tokenize
import PyPDF2
from numpy.linalg import norm
import re
import nltk

nltk.download('punkt')
nltk.download('punkt_tab')

class Doc2VecTrainer:

    def __init__(self, vector_size=50, min_count=5, epochs=40, alpha=0.001):
        self.vector_size = vector_size
        self.min_count = min_count
        self.epochs = epochs
        self.alpha = alpha

    def train(self, csv_path, model_path):

        df = pd.read_csv(csv_path)

        df['experience'] = "Experiences requises : " + df['experience'].astype(str)
        df['competences'] = "Compétences requises : " + df['competences'].astype(str)
        df['education'] = "Formation requise : " + df['education'].astype(str)

        df['data'] = df[['intitule_poste','education','competences','experience']].apply(
            lambda x: ' '.join(x.dropna().astype(str)), axis=1
        )

        tagged_data = [
            TaggedDocument(words=word_tokenize(text.lower()), tags=[str(i)])
            for i, text in enumerate(df['data'])
        ]

        model = Doc2Vec(
            vector_size=self.vector_size,
            min_count=self.min_count,
            epochs=self.epochs,
            alpha=self.alpha
        )

        model.build_vocab(tagged_data)

        for epoch in range(self.epochs):
            model.train(tagged_data,
                        total_examples=model.corpus_count,
                        epochs=1)

        model.save(model_path)
        return model

    

class CVMatcher:

    def __init__(self, model_path):
        self.model = Doc2Vec.load(model_path)

    def extract_text_from_pdf(self, pdf_path):
        pdf = PyPDF2.PdfReader(pdf_path)
        text = ""
        for page in pdf.pages:
            text += page.extract_text() or ""
        return re.sub(r'\s+', ' ', text).lower()

    def similarity(self, v1, v2):
        return 100 * (np.dot(v1, v2) / (norm(v1) * norm(v2)))

    def match_all_jobs(self, cv_pdf, jobs_csv, top_n=5):

        df = pd.read_csv(jobs_csv)

        cv_text = self.extract_text_from_pdf(cv_pdf)
        cv_vec = self.model.infer_vector(word_tokenize(cv_text))

        scores = []

        for desc in df['description']:
            jd_vec = self.model.infer_vector(word_tokenize(str(desc).lower()))
            sim = self.similarity(cv_vec, jd_vec)
            scores.append(round(sim, 2))

        df['similarity_score'] = scores

        df_top = df.sort_values("similarity_score", ascending=False).head(top_n).copy()

        # Optional: reset the index to ensure integer indexing
        df_top.reset_index(drop=True, inplace=True)
        print(df_top.columns)
        return df_top
