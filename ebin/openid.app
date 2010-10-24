{application,
 openid,
 [{description, "OpenID 2.0 implementation"},
  {vsn, "0.0.1"},
  {modules,
    [
      openid,
      openid_utils,
      openid_srv,
      yadis
    ]},
  {registered,[]},
  {applications,[kernel, stdlib]},
  {env,[]}]}.
