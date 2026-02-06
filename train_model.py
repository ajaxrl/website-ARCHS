import os

dir = os.path.dirname(os.path.realpath(__file__))
os.chdir(dir)

from classes.my_classes import Doc2VecTrainer

Trainer = Doc2VecTrainer()
Trainer.train("data/jobs.csv", "model/cv_job_matching.model")
