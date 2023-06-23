---
title: '恢复 mysql 中 root 的权限'
tags:
  - mysql
categories:
  - MySQL
date: 2021-09-16 16:36:00
---

晚上干活一时疏忽把 `root` 的权限收回了

```mysql
REVOKE ALL PRIVILEGES ON * . * FROM 'root'@'localhost';
REVOKE GRANT OPTION ON * . * FROM 'root'@'localhost';
```

上面命令带来的后果就是所有的操作都被拒绝，执行 `show databases` 只有 `information_schema` 显示。

```
mysql> use mysql;
ERROR 1044 (42000): Access denied for user 'root'@'localhost' to database 'mysql'
```

网上找到的解决方法均不适用于当前情况，特此记录。

<!-- more -->

## 第一步：使用 `--skip-grant-table` 跳过密码登录
1. `service mysqld stop` 停止 mysql 服务
2. `mysqld_safe --skip-grant-table` 使用安全模式跳过密码
3. 新开一个 terminal 输入 `mysql -u root` 登录

此时执行 `show databases` 应该能看到所有可用的数据库。

## 第二步：重新授予权限
1. 执行 `flush privileges`，是的没错，要先刷新权限，否则会得到 `The MySQL server is running with the --skip-grant-tables option so it cannot execute this statement` 的错误
2. 授予权限：`GRANT ALL PRIVILEGES ON *.* TO 'root'@'localhost' IDENTIFIED BY 'your_password' WITH GRANT OPTION`

重启 *mysql* 后即可正常使用。
