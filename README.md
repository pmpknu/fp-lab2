# Построение пользовательских типов данных на OCaml

## Лабораторная работа #2

- **Вариант:** `bt_bag`
- **Преподаватель** Пенской Александр Владимирович  
- **Выполнил** `Горляков Даниил Петрович`, `367165`
- ИТМО, Санкт-Петербург, 2024

### Задание

В рамках лабораторной работы вам предлагается реализовать одну из предложенных
классических структур данных (список, дерево, __бинарное дерево__, hashmap, граф).

### Требования
1. Функции:
    * добавление и удаление элементов;
    * фильтрация;
    * отображение (`map`);
    * свертки (левая и правая);
    * структура должна быть моноидом.
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

### Ключевые элементы реализации

- Реализация модуля описана в [bt_bag.ml](./lib/bt_bag.ml)
- Для обеспечения полиморфизма используется параметризированая модель([`module Make`](./lib/bt_bag.ml#L29)) взятая по аналогии со стандартной библиотеки `Set`
```ocaml
module Make (Ord : OrderedType) : Bag with type key = Ord.t = struct
  type key = Ord.t

  type 'a btree =
    | Empty
    | Node of
        { left : 'a btree
        ; value : key
        ; count : int
        ; right : 'a btree
        }
  (* Далее -- реализаия интерфейса Bag *)
```

- [Добавление](./lib/bt_bag.ml#L48) элементов
```ocaml
  let rec add x bag =
    match bag with
    | Empty -> Node { left = Empty; value = x; count = 1; right = Empty }
    | Node { left; value = key; count; right } ->
      if Ord.compare x key = 0
      then Node { left; value = key; count = count + 1; right }
      else if Ord.compare x key < 0
      then Node { left = add x left; value = key; count; right }
      else Node { left; value = key; count; right = add x right }
  ;;
```
- [Удаление](./lib/bt_bag.ml#L91) элементов
```ocaml
  let rec remove x = function
    | Empty -> Empty
    | Node { left; value; count; right } ->
      (match Ord.compare x value with
       | 0 ->
         if count = 1
         then merge left right
         else Node { left; value; count = count - 1; right }
       | n when n < 0 -> Node { left = remove x left; value; count; right }
       | _ -> Node { left; value; count; right = remove x right })
  ;;
```
- [Фильтрация](./lib/bt_bag.ml#L114)
```ocaml
  let rec filter p = function
    | Empty -> Empty
    | Node { left; value; count; right } as t ->
      let l' = filter p left in
      let pv = p value in
      let r' = filter p right in
      if pv
      then
        if left == l' && right == r'
        then t
        else Node { left = l'; value; count; right = r' }
      else merge l' r'
  ;;
```
- Отображение [(`map`)](./lib/bt_bag.ml#L103))
```ocaml
  let rec map f = function
    | Empty -> Empty
    | Node { left; value; count; right } as t ->
      let l' = map f left in
      let v' = f value in
      let r' = map f right in
      if left == l' && value == v' && right == r'
      then t
      else Node { left = l'; value = v'; count; right = r' }
  ;;
```
- Свертка [левая](./lib/bt_bag.ml#L135)
```ocaml
  let rec fold_left f accu s =
    match s with
    | Empty -> accu
    | Node { left; value; count; right } ->
      fold_left f (f (fold_left f accu left) value count) right
  ;;
```
- Свертка [правая](./lib/bt_bag.ml#L128)
```ocaml
  let rec fold_right f bag accu =
    match bag with
    | Empty -> accu
    | Node { left; value; count; right } ->
      fold_right f left (f value count (fold_right f right accu))
  ;;
```
- Структура должна быть [моноидом](https://en.wikipedia.org/wiki/Monoid):
* [Нейтральность]((./test/test_property.ml#L18))
```ocaml
let merge_empty_is_neutral =
  QCheck.Test.make
    ~count
    ~name:"forall bags: bag merge Empty = Empty merge bag = bag"
    generate_bag
    (fun bag ->
       let empty = IntBag.empty in
       let merged1 = IntBag.merge bag empty in
       let merged2 = IntBag.merge empty bag in
       if merged1 === bag && merged2 === bag
       then true
       else (
         let debugprint s b =
           Printf.printf
             s
             (String.concat ", " (List.map string_of_int (IntBag.elements b)))
         in
         debugprint "Failing bag elements    : %s\n" bag;
         debugprint "Failing MERGED1 elements: %s\n" merged1;
         debugprint "Failing MERGED2 elements: %s\n" merged2;
         false))
;;
```
  * [Ассоциативность](./test/test_property.ml#L41)
```ocaml
let merge_is_associative =
  QCheck.Test.make
    ~count
    ~name:"forall bags: (a merge b) merge c = a merge (b merge c)"
    (QCheck.triple generate_bag generate_bag generate_bag)
    (fun (a, b, c) -> a >>= (b >>= c) === (a >>= b >>= c))
;;
```

### Тесты, отчет инструмента тестирования, метрики

- Для [unit](./test/test_bt_bag.ml) тестирования использован фреймворк Alcotest
- Для [property](./test/test_property.ml) тестирования использован фреймворк QCheck

### Выводы

В ходе выполнения данной лабораторной работы мною были изучены
такие понятия как функции высшего порядка, property-based тестирование и 
parametrized modules в OCaml.
С помощью данных инструментов была разработана библиотека для работы с 
мультисетом на основе бинарного дерева, которую я покрыл unit- и property-based тестами.

Основной сложностью стало написание property-based тестов, из-за малого количества примеров в документации а также нераспознаванием lsp и линтером фреймворка тестирования.
