<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Кузнецов Дмитро Сергійович КВ-21</p>
<p align="right"><b>Рік</b>: 2025</p>
 
## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом
(п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV
файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним
типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.

1. Визначити структури або утиліти для створення записів з таблиць (в залежності від
типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а
також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або
структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і
т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз,
який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було
передано у select . При цьому лямбда-вираз в якості ключових параметрів може
отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку
лише заданими значеннями (виконати фільтрування). Вибірка повертається у
вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від
варіанту):
- - структури у геш-таблиці
- - геш-таблиці у асоціативні списки
- - асоціативні списки у геш-таблиці

6. Написати функцію(-ї) для "красивого" виводу записів таблиці.
   
## Варіант 7
База даних: Наукові статті

Тип записів: Структура

Таблиці: Спеціальності, Наукові статті
  
## Лістинг реалізації завдання
```lisp
(defstruct specialty
  id
  name)

(defstruct article
  id
  title
  author
  year
  specialty-id)

(defun split-csv-line (line)
  (loop with start = 0
        for pos = (position #\, line :start start)
        collect (string-trim " " (subseq line start pos))
        while pos
        do (setf start (1+ pos))))

(defun read-csv (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil nil)
          while line
          collect (split-csv-line line))))

(defun load-specialties (path)
  (let ((rows (rest (read-csv path))))
    (mapcar #'make-specialty-from-row rows)))

(defun make-specialty-from-row (row)
  (make-specialty
   :id (parse-integer (first row))
   :name (second row)))

(defun make-article-from-row (row)
  (make-article
   :id (parse-integer (first row))
   :title (second row)
   :author (third row)
   :year (parse-integer (fourth row))
   :specialty-id (parse-integer (fifth row))))

(defun select (path row->record)
  (lambda (&key id title author year specialty-id)
    (let* ((rows (rest (read-csv path)))
           (records (mapcar row->record rows)))
      (remove-if-not
       (lambda (rec)
         (and
          (if id (= id (article-id rec)) t)
            (if title (string= title (article-title rec)) t)
            (if author (string= author (article-author rec)) t)
            (if year (= year (article-year rec)) t)
            (if specialty-id (= specialty-id (article-specialty-id rec))

              t)))
       records))))

(defparameter *select-articles*
  (select "articles.csv" #'make-article-from-row))

(defun find-specialty-name (specialty-id specialties)
  (let ((spec (find specialty-id specialties
                    :key #'specialty-id
                    :test #'=)))
    (if spec
        (specialty-name spec)
        "Unknown specialty")))

(defun write-records-to-file (records path printer)
  (with-open-file (out path
                       :direction :output
                       :if-exists :supersede)
    (dolist (r records)
      (funcall printer r out))))
(defun article->csv (a stream)
  (format stream "~a,~a,~a,~a,~a~%"
          (article-id a)
          (article-title a)
          (article-author a)
          (article-year a)
          (article-specialty-id a)))

(defun article->hash (a)
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash 'id h) (article-id a)
          (gethash 'title h) (article-title a)
          (gethash 'author h) (article-author a)
          (gethash 'year h) (article-year a)
          (gethash 'specialty-id h) (article-specialty-id a))
    h))

(defun specialty->hash (s)
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash 'id h) (specialty-id s)
          (gethash 'name h) (specialty-name s))
    h))

(defun print-article-with-specialty (article specialties)
  (format t
          "~%--------------------------------~%~
           ID: ~a~%~
           Name: ~a~%~
           Author: ~a~%~
           Year: ~a~%~
           Specialty: ~a~%"
          (article-id article)
          (article-title article)
          (article-author article)
          (article-year article)
          (find-specialty-name
           (article-specialty-id article)
           specialties)))

(defun print-articles-with-specialties (articles specialties)
  (dolist (a articles)
    (print-article-with-specialty a specialties)))

(defparameter *articles-1*
  (funcall *select-articles* :specialty-id 1))

(defparameter *articles-2*
  (funcall *select-articles* :specialty-id 2))

(defparameter *articles*
  (append *articles-1* *articles-2*))

(defparameter *specialties*
  (load-specialties "specialties.csv"))

(print-articles-with-specialties *articles* *specialties*)

(write-records-to-file
 *articles*
 "result.csv"
 #'article->csv)
```
### Тестові набори та утиліти
```lisp
(defun check-read-test (test-name file-path row->record expected)
  (let* ((rows (rest (read-csv file-path)))
         (records (mapcar row->record rows))
         (result (equalp records expected)))
    (format t "Test: ~A - ~:[FAILED~;PASSED~]~%" test-name result)))

(defun check-select-test (test-name select-lambda filter-alist expected)
  (let ((result (funcall select-lambda
                         :id (getf filter-alist :id)
                         :title (getf filter-alist :title)
                         :author (getf filter-alist :author)
                         :year (getf filter-alist :year)
                         :specialty-id (getf filter-alist :specialty-id))))
    (format t "Test: ~A - ~:[FAILED~;PASSED~]~%" test-name
            (equalp result expected))))
(defun run-tests ()
  (check-read-test "Read specialties"
                   "specialties.csv"
                   #'make-specialty-from-row
                   (list (make-specialty :id 1 :name "Computer Science")
                         (make-specialty :id 2 :name "Applied Mathematics")
                         (make-specialty :id 3 :name "Physics")))
  
  (check-read-test "Read articles"
                   "articles.csv"
                   #'make-article-from-row
                   (list (make-article :id 1 :title "Lisp in Artificial Intelligence" :author "John McCarthy" :year 1960 :specialty-id 1)
                         (make-article :id 2 :title "Structure and Interpretation of Computer Programs" :author "Harold Abelson" :year 1985 :specialty-id 1)
                         (make-article :id 3 :title "Lambda Calculus and Functional Programming" :author "Alonzo Church" :year 1936 :specialty-id 2)
                         (make-article :id 4 :title "Computational Methods in Mathematics" :author "John von Neumann" :year 1947 :specialty-id 2)
                         (make-article :id 5 :title "Quantum Theory Foundations" :author "Max Planck" :year 1900 :specialty-id 3)
                         (make-article :id 6 :title "Relativity and Physics" :author "Albert Einstein" :year 1915 :specialty-id 3)))
  
  (check-select-test "Select article by ID"
                     (select "articles.csv" #'make-article-from-row)
                     '(:id 2)
                     (list (make-article :id 2 :title "Structure and Interpretation of Computer Programs" :author "Harold Abelson" :year 1985 :specialty-id 1)))

  (check-select-test "Select articles by specialty-id"
                     (select "articles.csv" #'make-article-from-row)
                     '(:specialty-id 2)
                     (list (make-article :id 3 :title "Lambda Calculus and Functional Programming" :author "Alonzo Church" :year 1936 :specialty-id 2)
                           (make-article :id 4 :title "Computational Methods in Mathematics" :author "John von Neumann" :year 1947 :specialty-id 2)))
  
  (format t "Full table of articles:~%")
  (print-articles-with-specialties
   (funcall (select "articles.csv" #'make-article-from-row))
   (load-specialties "specialties.csv")))
```
(run-tests)
```
### Тестування
```lisp
Test: Read specialties - PASSED
Test: Read articles - PASSED
Test: Select article by ID - PASSED
Test: Select articles by specialty-id - PASSED
Full table of articles:

--------------------------------
ID: 1
Name: Lisp in Artificial Intelligence
Author: John McCarthy
Year: 1960
Specialty: Computer Science

--------------------------------
ID: 2
Name: Structure and Interpretation of Computer Programs
Author: Harold Abelson
Year: 1985
Specialty: Computer Science

--------------------------------
ID: 3
Name: Lambda Calculus and Functional Programming
Author: Alonzo Church
Year: 1936
Specialty: Applied Mathematics

--------------------------------
ID: 4
Name: Computational Methods in Mathematics
Author: John von Neumann
Year: 1947
Specialty: Applied Mathematics

--------------------------------
ID: 5
Name: Quantum Theory Foundations
Author: Max Planck
Year: 1900
Specialty: Physics

--------------------------------
ID: 6
Name: Relativity and Physics
Author: Albert Einstein
Year: 1915
Specialty: Physics
```
