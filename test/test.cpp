#include "h2unit.h"
#include "../list_head.h"

typedef struct {
    int a;
    float b;
    list_head link;
    char c[343];
} list_head_host_t;

H2UNIT(list_head)
{
    list_head root; // root <-> h1 <-> h2 <-> h3 
    list_head root123;

    list_head_host_t h1, h2, h3;

    void setup() {
        list_init(&root123);

        h1.a = 1;
        list_init(&h1.link);
        list_add_tail(&root123, &h1.link);
        h2.a = 2;
        list_init(&h2.link);
        list_add_tail(&root123, &h2.link);
        h3.a = 3;
        list_init(&h3.link);
        list_add_tail(&root123, &h3.link);

        list_init(&root);
    }

    void teardown() {
    }
};

H2CASE(list_head, "sizeof")
{
    H2EQ(sizeof(void *) * 2, sizeof(list_head));
}

H2CASE(list_head, "rotate left")
{
    list_rotate_left(&root123);
    int a[] = {2, 3, 1};
    list_for_each_entry(p, &root123, list_head_host_t, link, i) {
        H2EQ(a[i], p->a);
    }
}

H2CASE(list_head, "rotate right")
{
    list_rotate_right(&root123);
    int a[] = {3, 1, 2};
    list_for_each_entry(p, &root123, list_head_host_t, link, i) {
        H2EQ(a[i], p->a);
    }
}

H2CASE(list_head, "splice")
{
    list_splice(&root, &root123);
    int a = 0;
    list_for_each_entry(p, &root, list_head_host_t, link, i) {
        H2EQ(++a, p->a);
    }
}

H2CASE(list_head, "splice 2")
{
    list_head_host_t h0;
    h0.a = 0;
    list_init(&h0.link);
    list_add_tail(&root, &h0.link);

    list_splice(&root, &root123);
    int a = 0;
    list_for_each_entry(p, &root, list_head_host_t, link) {
        H2EQ(a++, p->a);
    }
}

H2CASE(list_head, "replace")
{
    list_head_host_t h0;
    h0.a = 0;
    list_init(&h0.link);

    list_replace(&h1.link, &h0.link);

    int a[] = {0, 2, 3};
    list_for_each_entry(p, &root123, list_head_host_t, link, i) {
        H2EQ(a[i], p->a);
    }
}

H2CASE(list_head, "swap")
{
    list_head_host_t h0;
    h0.a = 0;
    list_init(&h0.link);
    list_add_tail(&root, &h0.link);

    list_swap(&h1.link, &h0.link);

    int a[] = {0, 2, 3};
    list_for_each_entry(p, &root123, list_head_host_t, link, i) {
        H2EQ(a[i], p->a);
    }

    int b[] = {1};
    list_for_each_entry(p, &root, list_head_host_t, link, i) {
        H2EQ(b[i], p->a);
    }
}

H2CASE(list_head, "list_for_each")
{
    int a[] = {1, 2, 3};

    int i = 0;
    list_for_each(p, &root123) {
        list_head_host_t *h = list_entry(p, list_head_host_t, link);
        H2EQ(a[i], h->a);
        i++;
    }

    list_for_each(p, &root123, j) {
        list_head_host_t *h = list_entry(p, list_head_host_t, link);
        list_out(p);
        H2EQ(a[j], h->a);
    }
}

H2CASE(list_head, "list_for_each_prev")
{
    int a[] = {3, 2, 1};
    int j = 0;
    list_for_each_prev(p, &root123) {
        list_head_host_t *h = list_entry(p, list_head_host_t, link);
        H2EQ(a[j++], h->a);
    }
    list_for_each_prev(p, &root123, k) {
        list_head_host_t *h = list_entry(p, list_head_host_t, link);
        H2EQ(a[k], h->a);
        list_out(p);
    }
}

H2CASE(list_head, "list_for_each_entry")
{
    int a[] = {1, 2, 3};
    int u = 0;
    list_for_each_entry(p, &root123, list_head_host_t, link) {
        list_out(&p->link);
        H2EQ(a[u++], p->a);
    }
    list_for_each_entry(p, &root123, list_head_host_t, link, y) {
        list_out(&p->link);
        H2EQ(a[y], p->a);
    }
}

H2CASE(list_head, "list_for_each_entry_prev")
{
    int a[] = {3, 2, 1};
    int ee = 0;
    list_for_each_entry_prev(p, &root123, list_head_host_t, link) {
        H2EQ(a[ee++], p->a);
    }
    list_for_each_entry_prev(p, &root123, list_head_host_t, link, i) {
        H2EQ(a[i], p->a);
        list_out(&p->link);
    }
}



typedef struct single_list_t {
    int a;
    struct single_list_t *next;
} single_list_t;

H2CASE(list_head, "single linked list utility")
{
    single_list_t s5 = {5, NULL};
    single_list_t s4 = {4, NULL};
    single_list_t s3 = {3, NULL};
    single_list_t s2 = {2, &s3};
    single_list_t s1 = {1, &s2};
    single_list_t *h = &s1;
    
    single_linked_list_add_after(&s3, &s4);
    single_linked_list_add_tail(h, &s5);

    int a[] = {1, 2, 3, 4, 5};
    single_linked_list_for_each(p, h, i) {
        H2EQ(a[i], p->a);
    }
    int j = 0;
    single_linked_list_for_each(p, h) {
        H2EQ(a[j++], p->a);
    }
}
