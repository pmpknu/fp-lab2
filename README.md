# Построение пользовательских типов данных на OCaml

## Лабораторная работа #2

- **Вариант:** `bt_bag`
- **Преподаватель** Пенской Александр Владимирович  
- **Выполнил** `Горляков Даниил Петрович`, `367165`
- ИТМО, Санкт-Петербург, 2024

### Задание

В рамках лабораторной работы вам предлагается реализовать одну из предложенных
классических структур данных (список, дерево, __бинарное дерево__, hashmap, граф).

#### Требования
1. Функции:
    * [ ] добавление и удаление элементов;
    * [ ] фильтрация;
    * [ ] отображение (`map`);
    * [ ] свертки (левая и правая);
    * [ ] структура должна быть моноидом.
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования
(как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования.
Примечание: некоторые языки позволяют получить большую часть API через
реализацию небольшого интерфейса.
Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо
реализовать их вручную и по возможности -- обеспечить совместимость.

`Bag` -- интерфейс, `Binary btree` -- структура данных