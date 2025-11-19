# Tasks - RStudio (University Exercises)

This repository contains a collection of R tasks and course exercises I completed using R / RStudio as part of my university courses. Each top-level folder corresponds to a task and contains the R scripts / R Markdown files and the rendered outputs (PDF or HTML), plus the data used for the exercises.

Below I summarise what each directory contains and a short note about what I learned from the PDF/HTML (or the R script) in that folder.

## Repository structure (top-level folders)

- 01 intro R
- 02 intro R
- 03 TABULAR
- 04 TABULAR
- 05 TABULAR
- 06 TABULAR
- 07 CONTR
- 08 CONTR

---

## 01 intro R
Files of note:
- tarea1INTROR.pdf — https://github.com/julioruizm14/Tasks-RStudio/blob/main/01%20intro%20R/tarea1INTROR.pdf
- tarea1INTROR.Rmd — https://github.com/julioruizm14/Tasks-Rudio/blob/main/01%20intro%20R/tarea1INTROR.Rmd (source)
- iris.csv, tratamiento.csv — example datasets

What I learned:
- Basic R syntax and RStudio workflow: running scripts, using R Markdown to produce reports (PDF).
- How to import CSV data into R and inspect data frames (head, str, summary).
- Basic exploratory data analysis: summary statistics, simple plots (histograms, boxplots, scatterplots).
- Simple data transformations (creating factors, subsetting rows/columns).

---

## 02 intro R
Files of note:
- tarea2INTROR.R — https://github.com/julioruizm14/Tasks-RStudio/blob/main/02%20intro%20R/tarea2INTROR.R
- biom2003.csv — dataset used in the exercise

What I learned:
- Continued practice with data import and cleaning using base R and scripts.
- Practice writing reproducible R scripts that perform a sequence of analyses (data cleaning → summaries → plots).
- Getting more comfortable with vectorized operations, indexing, and basic plotting customisations.

---

## 03 TABULAR
Files of note:
- Tarea1TABULAR.pdf — https://github.com/julioruizm14/Tasks-RStudio/blob/main/03%20TABULAR/Tarea1TABULAR.pdf
- Tarea1TABULAR.Rmd — source R Markdown
- autoevaluacion1.pdf — https://github.com/julioruizm14/Tasks-RStudio/blob/main/03%20TABULAR/autoevaluacion1.pdf
- tiendas.csv — example tabular dataset

What I learned:
- Working with tabular data at scale: reading/writing CSVs, checking data types, handling categorical variables.
- Data wrangling basics: filtering, selecting, arranging, creating new summary columns.
- Generating summary tables and visualisations to understand patterns in tabular datasets.

---

## 04 TABULAR
Files of note:
- Tarea2TABULAR.pdf — https://github.com/julioruizm14/Tasks-RStudio/blob/main/04%20TABULAR/Tarea2TABULAR.pdf
- autoevaluacion2.pdf — https://github.com/julioruizm14/Tasks-RStudio/blob/main/04%20TABULAR/autoevaluacion2.pdf
- vuelos.csv — large flights dataset for practice

What I learned:
- Handling larger tabular datasets (performance considerations, readr vs base read.csv).
- Grouped summaries and aggregations (group_by + summarize, or aggregate) to extract insights from many rows.
- Dealing with missing or inconsistent values and performing basic data cleaning before analysis.

---

## 05 TABULAR
Files of note:
- Tarea3TABULAR.pdf — https://github.com/julioruizm14/Tasks-RStudio/blob/main/05%20TABULAR/Tarea3TABULAR.pdf
- autoevaluacion3.pdf — https://github.com/julioruizm14/Tasks-RStudio/blob/main/05%20TABULAR/autoevaluacion3.pdf

What I learned:
- More advanced tabular manipulations: reshaping (wide ↔ long), joins/merges between tables.
- Creating reproducible reports combining code, results and narrative (R Markdown).
- Formulating and answering specific questions using grouped analyses and visual summaries.

---

## 06 TABULAR
Files of note:
- Tarea4TABULAR.pdf — https://github.com/julioruizm14/Tasks-RStudio/blob/main/06%20TABULAR/Tarea4TABULAR.pdf
- autoevaluacion4.Rmd / .tex / .log — sources and build artifacts

What I learned:
- Consolidation of tabular data skills: thorough data cleaning, advanced grouping and multi-table workflows.
- Exporting results and automated report generation (producing PDFs via R Markdown / LaTeX).
- Debugging reproducible builds (interpreting logs / LaTeX artifacts when rendering fails).

---

## 07 CONTR
Files of note:
- Tarea1CONTR.html — https://github.com/julioruizm14/Tasks-RStudio/blob/main/07%20CONTR/Tarea1CONTR.html
- Tarea1CONTR.Rmd — source R Markdown
- medicamentos.csv — dataset used in the exercises

What I learned:
- Statistical contrasts and hypothesis testing basics (interpreting group differences).
- Performing and interpreting t-tests, ANOVA or similar group-comparison procedures (depending on the exercise).
- Presenting statistical test results clearly in an HTML report (tables, test statistics, p-values, and conclusions).

---

## 08 CONTR
Files of note:
- Tarea2CONTR.html — https://github.com/julioruizm14/Tasks-RStudio/blob/main/08%20CONTR/Tarea2CONTR.html
- Tarea2CONTR.Rmd — source R Markdown
- dieta.csv, william.csv — example datasets

What I learned:
- More practice with contrasts and post-hoc comparisons; possibly non-parametric alternatives where assumptions are not met.
- Applying tests to real datasets and writing up concise interpretations for each result.
- Creating reproducible HTML reports showing code, outputs and discussion.

---

## Notes & how to reproduce
- Most exercises were authored as R Markdown documents (.Rmd) and rendered to PDF or HTML. The rendered outputs are included in each folder (PDF/HTML), and the corresponding .Rmd or .R scripts are the sources.
- To reproduce any report:
  1. Open the corresponding .Rmd or .R file in RStudio.
  2. Make sure required packages (tidyverse, knitr, rmarkdown, etc.) are installed.
  3. Knit the document to produce the same PDF/HTML output.

---

If you want, I can:
- Add badges, a license, or a detailed list of packages used per task.
- Generate a single index page linking directly to each rendered PDF/HTML for easier navigation.
- Expand any directory summary with more detail (for example, list the exact functions or tests used) after extracting the Rmd/R source.

Author: julioruizm14
