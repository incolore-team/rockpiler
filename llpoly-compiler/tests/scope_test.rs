use rockc::ir::{
    scope::{ScopeCursor, ScopeId, ScopeRegistry},
    value::ValueId,
};

#[test]
fn test_scope() {
    /*
    + value: name = value_1, id = 0
    > should resove value_1 to id = 0
    + scope1:

    +--- value: name = value_1, id = 1
    >    should resolve value_1 to id = 1

    +--- value: name = value_2, id = 2
    >    should resolve value_2 to id = 2

    +--- scope2:

    +------ value: name = value_3, id = 3
    >       should resolve value_3 to id = 3

    +------ value: name = value_2, id = 4
    >       should resolve value_2 to id = 4

    +--------- scope3:

    +------------ value: name = value_1, id = 5
    >             should resolve value_1 to id = 5

    +------------ value: name = value_2, id = 6
    >             should resolve value_2 to id = 6

    +------------ value: name = value_3, id = 7
    >             should resolve value_3 to id = 7

    +-------value: name = value_2, id = 8
    >       should resolve value_2 to id = 8

    +-------value: name = value_4, id = 9
    >       should resolve value_4 to id = 9
    >       should resolve value_1 to id = 1
    >       should resolve value_2 to id = 8
    >       should resolve value_3 to id = 3

    +----value: name = value_5, id = 10
    >    should resolve value_5 to id = 10
    >    should resolve value_1 to id = 1

    +----value: name = value_1, id = 11
    >    should resolve value_1 to id = 11
    > should resolve value_1 to id = 0
    */
    let mut ctx = ScopeRegistry::default();
    let scope_root = ScopeId::root();
    ctx.register(scope_root, "value_1".to_string(), ValueId::from(0));
    assert_eq!(ctx.resolve(scope_root, "value_1"), Some(ValueId::from(0)));
    {
        let scope1 = ctx.derive(scope_root);
        ctx.register(scope1, "value_1".to_string(), ValueId::from(1));
        assert_eq!(ctx.resolve(scope1, "value_1"), Some(ValueId::from(1)));
        ctx.register(scope1, "value_2".to_string(), ValueId::from(2));
        assert_eq!(ctx.resolve(scope1, "value_2"), Some(ValueId::from(2)));
        {
            let scope2 = ctx.derive(scope1);
            ctx.register(scope2, "value_3".to_string(), ValueId::from(3));
            assert_eq!(ctx.resolve(scope2, "value_3"), Some(ValueId::from(3)));
            ctx.register(scope2, "value_2".to_string(), ValueId::from(4));
            assert_eq!(ctx.resolve(scope2, "value_2"), Some(ValueId::from(4)));
            {
                let scope3 = ctx.derive(scope2);
                ctx.register(scope3, "value_1".to_string(), ValueId::from(5));
                assert_eq!(ctx.resolve(scope3, "value_1"), Some(ValueId::from(5)));
                ctx.register(scope3, "value_2".to_string(), ValueId::from(6));
                assert_eq!(ctx.resolve(scope3, "value_2"), Some(ValueId::from(6)));
                ctx.register(scope3, "value_3".to_string(), ValueId::from(7));
                assert_eq!(ctx.resolve(scope3, "value_3"), Some(ValueId::from(7)));
                ctx.leave(scope3);
            }
            ctx.register(scope2, "value_2".to_string(), ValueId::from(8));
            assert_eq!(ctx.resolve(scope2, "value_2"), Some(ValueId::from(8)));
            ctx.register(scope2, "value_4".to_string(), ValueId::from(9));
            assert_eq!(ctx.resolve(scope2, "value_4"), Some(ValueId::from(9)));
            assert_eq!(ctx.resolve(scope2, "value_1"), Some(ValueId::from(1)));
            assert_eq!(ctx.resolve(scope2, "value_2"), Some(ValueId::from(8)));
            assert_eq!(ctx.resolve(scope2, "value_3"), Some(ValueId::from(3)));
            ctx.leave(scope2);
        }
        ctx.register(scope1, "value_5".to_string(), ValueId::from(10));
        assert_eq!(ctx.resolve(scope1, "value_5"), Some(ValueId::from(10)));
        assert_eq!(ctx.resolve(scope1, "value_1"), Some(ValueId::from(1)));
        ctx.register(scope1, "value_1".to_string(), ValueId::from(11));
        assert_eq!(ctx.resolve(scope1, "value_1"), Some(ValueId::from(11)));
        ctx.leave(scope1);
    }
    assert_eq!(ctx.resolve(scope_root, "value_1"), Some(ValueId::from(0)));
}
#[test]
fn test_scope_cursor() {
    /*
       + scope1:
       +--- value: name = value_1, id = 1
       +--- scope2:
       +--------- scope3:
       +------------ value: name = value_1, id = 5
       +--------- scope4:
       +------------ value: name = value_1, id = 6
       +--- scope5:
       +--------- value: name = value_1, id = 7
    */

    let mut ctx = ScopeRegistry::default();
    let scope_root = ScopeId::root();
    {
        let scope1 = ctx.derive(scope_root);
        ctx.register(scope1, "value_1".to_string(), ValueId::from(1));
        {
            let scope2 = ctx.derive(scope1);
            {
                let scope3 = ctx.derive(scope2);
                ctx.register(scope3, "value_1".to_string(), ValueId::from(5));
                assert_eq!(ctx.leave(scope3), scope2);
            }
            {
                let scope4 = ctx.derive(scope2);
                ctx.register(scope4, "value_1".to_string(), ValueId::from(6));
                assert_eq!(ctx.leave(scope4), scope2);
            }
            ctx.leave(scope2);
        }
        {
            let scope5 = ctx.derive(scope1);
            ctx.register(scope5, "value_1".to_string(), ValueId::from(7));
            assert_eq!(ctx.leave(scope5), scope1);
        }
        assert_eq!(ctx.leave(scope1), scope_root);
    }
    let mut cursor = ScopeCursor::new(&ctx);
    let mut s;
    assert_eq!(cursor.scope().id, scope_root);
    assert_eq!(cursor.scope().parent_id, scope_root);
    print!("{:?}", cursor.scope().children_ids);
    assert_eq!(cursor.scope().children_ids.len(), 1);
    assert_eq!(cursor.scope().sym_map.len(), 0);

    s = cursor.enter_child();
    assert!(s.is_some());
    assert_eq!(cursor.scope().id, ScopeId::from(1));
    assert_eq!(cursor.scope().parent_id, scope_root);
    assert_eq!(cursor.scope().children_ids.len(), 2);
    assert_eq!(cursor.scope().sym_map.len(), 1);
    assert_eq!(
        cursor.scope().sym_map.get("value_1"),
        Some(&ValueId::from(1))
    );

    s = cursor.enter_child();
    assert!(s.is_some());
    assert_eq!(cursor.scope().id, ScopeId::from(2));
    assert_eq!(cursor.scope().parent_id, ScopeId::from(1));
    assert_eq!(cursor.scope().children_ids.len(), 2);
    assert_eq!(cursor.scope().sym_map.len(), 0);

    s = cursor.enter_child();
    assert!(s.is_some());
    assert_eq!(cursor.scope().id, ScopeId::from(3));
    assert_eq!(cursor.scope().parent_id, ScopeId::from(2));
    assert_eq!(cursor.scope().children_ids.len(), 0);
    assert_eq!(cursor.scope().sym_map.len(), 1);

    s = cursor.enter_next();
    assert!(s.is_some());
    assert_eq!(cursor.scope().id, ScopeId::from(4));
    assert_eq!(cursor.scope().parent_id, ScopeId::from(2));
    assert_eq!(cursor.scope().children_ids.len(), 0);
    assert_eq!(cursor.scope().sym_map.len(), 1);

    s = cursor.leave();
    assert!(s.is_some());
    assert_eq!(cursor.scope().id, ScopeId::from(2));

    s = cursor.enter_next();
    assert!(s.is_some());
    assert_eq!(cursor.scope().id, ScopeId::from(5));

    s = cursor.enter_child();
    assert!(s.is_none());

    s = cursor.enter_next();
    assert!(s.is_none());

    s = cursor.leave();
    assert!(s.is_some());
    assert_eq!(cursor.scope().id, ScopeId::from(1));
}
