# LMUK-Geo: A Dataset and LLM-Driven Geoparsing Approach for UK Local News

This repository contains the code used in our forthcoming journal article "Towards Efficient and Accessible Geoparsing of
UK Local Media: A Benchmark Dataset and LLM-based Approach" which introduces the LMUK-Geo dataset and a novel, scalable, accessible, and robust LLM-driven geoparsing approach for UK local news. The dataset itself is available on Harvard Dataverse: [https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SGVXIU](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SGVXIU)

## Introduction

This project addresses the need for improved geoparsing of local media, focusing on the under-explored geographic context of the UK.  We introduce two key contributions:

1. **The Local Media UK Geoparsing (LMUK-Geo) dataset:** A novel, annotated gold standard corpus of 182 UK local news articles, enabling the development and evaluation of tailored geoparsing models.  The dataset is hosted on Harvard Dataverse.
2. **A novel LLM-driven geoparsing approach:**  This approach tackles the challenges of location disambiguation and contextual understanding in UK local news.

This repository provides the code used for our LLM-driven geoparsing approach, facilitating reproducibility and further research in this area.  The LMUK-Geo dataset must be downloaded from Harvard Dataverse before running the code.

## Dataset

The LMUK-Geo dataset consists of 182 UK local news articles sourced from the UKTwitNewsCor corpus.  It is annotated with toponyms (GPE, LOC, FAC) and their corresponding Local Authority Districts (LADs).  The annotation process involved toponym recognition using Prodigy and SpaCy, candidate generation using Ordnance Survey Open Names and OpenStreetMap Nominatim, and manual disambiguation using LabelStudio.  The dataset is designed to capture fine-grained locations prevalent in local news, excluding larger-scale geographic references.

* **Location:** [https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SGVXIU](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SGVXIU)
* **Format:**  JSON and CSV. 

## Prompt-Based Geoparsing Approach

Our approach utilises open-source LLMs (Gemma2, Llama3.1, Qwen2, Mistral) via the Ollama framework within an R environment. We explored different prompting strategies, including contextual toponym disambiguation from a knowledge base and few-shot LAD classification.  We also investigated the impact of varying metadata context (outlet name, outlet coverage district, other toponyms in the article) on LLM geoparsing performance.  Majority voting was used to enhance robustness.

## Evaluation

We evaluated our approach using both classification metrics (Accuracy) and distance metrics (Mean Error Distance, Accuracy@20km, Accuracy@161km). We compared our results against a baseline provided by [Hu et al, 2024](https://www-tandfonline-com.surrey.idm.oclc.org/doi/full/10.1080/13658816.2024.2405182?af=R).  We considered different evaluation scenarios, including handling edge cases and using majority voting.

## Repository Structure

The repository is organised as follows:

* `scripts/`: Contains the R scripts used for creating the dataset, geoparsing, analysis, and evaluation.
* `files/`: Contains the data files used in the project, including (but not limited to) raw data, intermediate processed data, and output files.  

## Citation

If you use this dataset or code in your research, please cite the following:
```
@article{bisiani_2025_lmuk,
  title={Towards Efficient and Accessible Geoparsing of UK Local Media: A Benchmark Dataset and LLM-based Approach},
  author={Bisiani, S., Gulyas, A., and Bahareh Heravi},
  journal={Computational Humanities Research (Submitted)},
  year={2025}, 
}
```
