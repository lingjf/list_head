
#ifndef ___LIST_HEAD_H___
#define ___LIST_HEAD_H___


typedef struct list_head {
    struct list_head *next, *prev;
} list_head;

#define LIST_HEAD_INIT(obj) { &(obj), &(obj) }

/* For the entry to be added , INIT is not required */
#define ___list_add_between(entry,before,after) list_head *new_= (entry), *prev = (before), *next = (after); (next)->prev = (new_); (new_)->next = (next); (new_)->prev = (prev); (prev)->next = (new_)

#define ___list_del_between(_prev,_next)          \
        (_prev)->next = (_next);                  \
        (_next)->prev = (_prev)
        


/* void list_init(struct list_head *entry) */
#define list_init(entry) (entry)->next = (entry)->prev = (entry)

/* void list_add_after(struct list_head *base, struct list_head *entry) */ 
#define list_add_after(base, entry) do { ___list_add_between((entry),(base),(base)->next); } while(0)

/* void list_add_before(struct list_head *base, struct list_head *entry) */
#define list_add_before(base, entry) do { ___list_add_between((entry),(base)->prev,(base)); } while(0)

/* void list_add_head(struct list_head *head, struct list_head *entry) */
#define list_add_head(head, entry) do { ___list_add_between((entry),(head),(head)->next); } while(0)

/* void list_add_tail(struct list_head *head, struct list_head *entry) */
#define list_add_tail(head, entry) do { ___list_add_between((entry),(head)->prev,(head)); } while(0)


/* void list_out(struct list_head *entry) */
// #define list_out(entry) do { list_head *p = entry; ___list_del_between(p->prev, p->next); list_init(p); } while(0)
#define list_out(entry) do { (entry)->prev->next = (entry)->next; (entry)->next->prev = (entry)->prev; (entry)->next = (entry)->prev = (entry);} while(0)

/* bool list_empty(struct list_head *head) */
#define list_empty(head) ((head)->next == (head))


/* bool list_is_first(struct list_head *head, struct list_head *entry) */
#define list_is_first(head, entry) ((entry)->prev == (head))

/* bool list_is_last(struct list_head *head, struct list_head *entry) */
#define list_is_last(head, entry) ((entry)->next == (head))


/* struct list_head *list_get_first(struct list_head *head) */
#define list_get_first(head) (list_empty(head) ? (list_head*)0 : (head)->next)

/* struct list_head *list_get_last(struct list_head *head) */
#define list_get_last(head) (list_empty(head) ? (list_head*)0 : (head)->prev)


/* void list_enqueue(struct list_head *head, struct list_head *entry) */
#define list_enqueue list_add_tail

/* struct list_head *list_dequeue(struct list_head *head, struct list_head *entry) */
#define list_dequeue(head, entry) do {                          \
    if (list_empty(head)) {                                     \
        entry = (list_head *) 0;                                \
    } else {                                                    \
        entry = (head)->next;                                   \
        list_out(entry);                                        \
    }                                                           \
} while(0)


/* void list_push(struct list_head *head, struct list_head *entry) */
#define list_push list_add_tail

/* struct list_head *list_pop(struct list_head *head, struct list_head *entry) */
#define list_pop(head, entry) do {                              \
    if (list_empty(head)) {                                     \
        entry = (list_head *) 0;                                \
    } else {                                                    \
        entry = (head)->prev;                                   \
        list_out(entry);                                        \
    }                                                           \
} while(0)


/* void list_splice(struct list_head *to, struct list_head *from) */
#define list_splice(to, from) do {                              \
    if (!list_empty(from)) {                                    \
        struct list_head *first = (from)->next;                 \
	    struct list_head *last = (from)->prev;                  \
        struct list_head *prev = (to)->prev;                    \
		struct list_head *next = (to);                          \
        first->prev = prev;                                     \
        prev->next = first;                                     \
        last->next = next;                                      \
        next->prev = last;                                      \
        list_init(from);                                        \
    }                                                           \
} while(0)


/* void list_rotate_left(struct list_head *head) */
#define list_rotate_left(head) do {                             \
	if (!list_empty(head)) {                                    \
		struct list_head *first = (head)->next;                 \
        list_out(first);                                        \
		list_add_tail((head), first);                           \
	}                                                           \
} while(0)

/* void list_rotate_right(struct list_head *head) */
#define list_rotate_right(head) do {                            \
	if (!list_empty(head)) {                                    \
		struct list_head *last = (head)->prev;                  \
        list_out(last);                                         \
		list_add_head((head), last);                            \
	}                                                           \
} while(0)


/* void list_replace(struct list_head *old, struct list_head *entry) */
#define list_replace(old, entry) do {                           \
    ___list_add_between(entry,(old)->prev,(old)->next);         \
    list_init(old);                                             \
} while(0)

/* void list_swap(struct list_head *entry1, struct list_head *entry2) */
#define list_swap(entry1, entry2) do {                          \
    struct list_head *prev1 = (entry1)->prev;                   \
    struct list_head *next1 = (entry1)->next;                   \
    {___list_add_between(entry1,(entry2)->prev,(entry2)->next);}\
    {___list_add_between(entry2,prev1,next1);}                  \
} while(0)



#define list_first_entry(head, type, link) list_entry((head)->next, type, link)
#define list_last_entry(head, type, link) list_entry((head)->prev, type, link)
#define list_next_entry(ptr, type, link) list_entry((ptr)->link.next, type, link)
#define list_prev_entry(ptr, type, link) list_entry((ptr)->link.prev, type, link)

#define list_get_first_entry(head, type, link) (list_empty(head) ? (type*)0 : list_entry(((head)->next), type, link))
#define list_get_last_entry(head, type, link) (list_empty(head) ? (type*)0 : list_entry(((head)->prev), type, link))

#define list_top_entry(head, type, link) ((head)->empty() ? (type*)0 : list_entry((head)->last(), type, link))
#define list_pop_entry(head, type, link) ((head)->empty() ? (type*)0 : list_entry((head)->pop(), type, link))
#define list_dequeue_entry(head, type, link) ((head)->empty() ? (type*)0 : list_entry((head)->dequeue(), type, link))

#define list_entry(ptr, type, link) \
	((type *)((char *)(ptr)-(char *)(&(((type *)(1))->link))+1))


#define ___PP_CAT2(_1, _2) ___PP_INTERNAL_CAT2(_1, _2)
#define ___PP_INTERNAL_CAT2(_1, _2) _1##_2

#define ___ANONYMOUS_VARIABLE() ___PP_CAT2(___PP_CAT2(____, __COUNTER__), ___PP_CAT2(____, __LINE__))

#define ___PP_COUNTOF_ARG(...) ___PP_INTERNAL_9TH(_, ##__VA_ARGS__, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#define ___PP_INTERNAL_9TH(_, _9, _8, _7, _6, _5, _4, _3, _2, _1, _0, ...) _0

#define ___PP_VARIADIC_CALL(_Macro, ...) ___PP_CAT2(_Macro, ___PP_COUNTOF_ARG(__VA_ARGS__))(__VA_ARGS__)

#define list_for_each_unsafe(p, head) for (list_head *p = (head)->next; p != (list_head *)(head); p = p->next)

#define _list_for_each_safe3(p, t, head) \
    for (list_head *p = (head)->next, *t = p->next; p != (list_head *)(head); p = t, t = t->next)

#define _list_for_each_safe4(p, t, head, i) for (int i = 0; i == 0; ++i) \
    for (list_head *p = (head)->next, *t = p->next; p != (list_head *)(head); p = t, t = t->next, ++i)

#define list_for_each(p, head, ...) ___PP_VARIADIC_CALL(_list_for_each_safe, p, ___ANONYMOUS_VARIABLE(), head, ##__VA_ARGS__)

#define list_for_each_entry_unsafe(p, head, type, link) for (type *p = list_entry((head)->next, type, link); &p->link != (list_head *)(head); p = list_entry(p->link.next, type, link))

#define _list_for_each_entry_safe5(p, t, head, type, link) \
    for (type *p = list_entry((head)->next, type, link), *t = list_entry(p->link.next, type, link); &p->link != (list_head *)(head); p = t, t = list_entry(t->link.next, type, link))

#define _list_for_each_entry_safe6(p, t, head, type, link, i) for (int i = 0; i == 0; ++i) \
    for (type *p = list_entry((head)->next, type, link), *t = list_entry(p->link.next, type, link); &p->link != (list_head *)(head); p = t, t = list_entry(t->link.next, type, link), ++i)

#define list_for_each_entry(p, head, type, link, ...) ___PP_VARIADIC_CALL(_list_for_each_entry_safe, p, ___ANONYMOUS_VARIABLE(), head, type, link, ##__VA_ARGS__)

#define list_for_each_prev_unsafe(p, head) for (int i = 0; i == 0; ++i) \
    for (list_head *p = (head)->prev; p != (list_head *)(head); p = p->prev, ++i)

#define _list_for_each_prev_safe3(p, t, head) \
    for (list_head *p = (head)->prev, *t = p->prev; p != (list_head *)(head); p = t, t = t->prev)

#define _list_for_each_prev_safe4(p, t, head, i) for (int i = 0; i == 0; ++i) \
    for (list_head *p = (head)->prev, *t = p->prev; p != (list_head *)(head); p = t, t = t->prev, ++i)

#define list_for_each_prev(p, head, ...) ___PP_VARIADIC_CALL(_list_for_each_prev_safe, p, ___ANONYMOUS_VARIABLE(), head, ##__VA_ARGS__)

#define list_for_each_entry_prev_unsafe(p, head, type, link) for (int i = 0; i == 0; ++i) \
    for (type *p = list_entry((head)->prev, type, link); &p->link != (list_head *)(head); p = list_entry(p->link.prev, type, link), ++i)

#define _list_for_each_entry_prev_safe5(p, t, head, type, link) \
    for (type *p = list_entry((head)->prev, type, link), *t = list_entry(p->link.prev, type, link); &p->link != (list_head *)(head); p = t, t = list_entry(t->link.prev, type, link))

#define _list_for_each_entry_prev_safe6(p, t, head, type, link, i) for (int i = 0; i == 0; ++i) \
    for (type *p = list_entry((head)->prev, type, link), *t = list_entry(p->link.prev, type, link); &p->link != (list_head *)(head); p = t, t = list_entry(t->link.prev, type, link), ++i)

#define list_for_each_entry_prev(p, head, type, link, ...) ___PP_VARIADIC_CALL(_list_for_each_entry_prev_safe, p, ___ANONYMOUS_VARIABLE(), head, type, link, ##__VA_ARGS__)



static inline int list_count(list_head *head) {
    int c = 0;
    list_for_each_unsafe(p, head) {
        c++;
    }
    return c;
}


#define single_linked_list_add_after(base, entry) \
    do { (entry)->next = (base)->next; (base)->next = (entry); } while(0)

#define single_linked_list_add_tail(root, entry) \
    do { typeof(root) *pp = &(root); while (*pp) pp = &(*pp)->next; *pp = entry; } while(0)

#define _single_linked_list_for_each_safe3(p, t, root) \
    for (typeof(root) p = root, t; p && (t = p->next, 1); p = t)

#define _single_linked_list_for_each_safe4(p, t, root, i) for (int i = 0; i == 0; ++i) \
    for (typeof(root) p = root, t; p && (t = p->next, 1); p = t, ++i)
        
#define single_linked_list_for_each(p, root, ...) ___PP_VARIADIC_CALL(_single_linked_list_for_each_safe, p, ___ANONYMOUS_VARIABLE(), root, ##__VA_ARGS__)
    
    
#endif
