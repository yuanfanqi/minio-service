# minio-service
minio文件存储系统基本功能


下载地址：dl.min.io/server/minio/release/linux-amd64/minio
启动before：
chmod +x minio
export MINIO_ACCESS_KEY=minioadmin
export MINIO_SECRET_KEY=minioadmin
./minio server /home/minIO/data
==//展示
API: http://192.168.0.156:9000  http://127.0.0.1:9000           
RootUser: minioadmin
RootPass: minioadmin
Console: http://192.168.0.156:9001 http://127.0.0.1:9001      
RootUser: minioadmin
RootPass: minioadmin
==//展示 end//==
启动&打印日志:
nohup ./minio server --address :9000 --console-address :9001 /home/minIO/data > /home/minIO/data/minio.log &
查看开放端口命令：
firewall-cmd --zone=public --list-ports
开放端口命令：
firewall-cmd --zone=public --add-port=9000/tcp --permanent
firewall-cmd --zone=public --remove-port=9000/tcp --permanent
firewall-cmd --reload

====//配置文件//=======
#minio配置信息
#minio.bucket=file-test
minio.host=http://192.168.0.156:9000
minio.url=${minio.host}/${minio.bucket}/
minio.access-key=minioadmin
minio.secret-key=minioadmin
====//配置文件end//=======

====//依赖文件//=======
<!-- https://mvnrepository.com/artifact/io.minio/minio -->
        <dependency>
            <groupId>io.minio</groupId>
            <artifactId>minio</artifactId>
            <version>8.3.0</version>
        </dependency>

        <!-- https://mvnrepository.com/artifact/org.junit.jupiter/junit-jupiter-api -->
        <dependency>
            <groupId>org.junit.jupiter</groupId>
            <artifactId>junit-jupiter-api</artifactId>
            <version>5.8.2</version>
            <scope>test</scope>
        </dependency>
====//依赖文件end//=======


minio：快速存储平台（在k8s方便）//Kubernetes，也被称为K8s或Kube，是容器编排器。
纠删码：数据保护方式，可以在丢失或损坏n/2以下的数据时还原数据（矩阵）
位衰减保护：纠删码
minio是无中心节点的，随便哪个节点都能读写数据，可以使用nginx代理

存储桶可以简单理解为“根文件夹”，每个存储桶都是minio服务下的一个一级结点，其下可以有多个子文件夹。
在minio服务里每个存储内容都是一个对象
removeBucket不会删除存储桶里的对象，你需要通过removeObject API来删除它们

MinIO分布式集群是指在多个服务器节点均部署MinIO服务，并将其组建为分布式存储集群，对外提供标准S3接口以进行统一访问


/*
Navicat MySQL Data Transfer

Date: 2023-02-10 16:53:59
*/


-- ----------------------------
-- Table structure for `log_minio_operate`
-- ----------------------------
DROP TABLE IF EXISTS `log_minio_operate`;
CREATE TABLE `log_minio_operate` (
`id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT '主键',
`file_name` varchar(32) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL COMMENT '文件名',
`source_service` varchar(45) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL COMMENT '来源服务ID',
`addr_minio` varchar(50) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL COMMENT '文件存储路径',
`file_hash` varchar(100) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL COMMENT '文件hash编码',
`file_desc` varchar(50) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '文件描述',
`operate` char(2) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '操作(0-查询，1-上传，2-下载，3-删除)',
`replace_time` timestamp NULL DEFAULT NULL COMMENT '替换时间',
`is_delete` char(2) NOT NULL DEFAULT '0' COMMENT '是否删除(0-false;1-true)',
`create_time` timestamp NOT NULL COMMENT '创建时间',
`create_user` varchar(20) NOT NULL COMMENT '创建人员',
`modify_time` timestamp NULL DEFAULT NULL ON UPDATE CURRENT_TIMESTAMP COMMENT '修改时间',
`modify_user` varchar(20) DEFAULT NULL COMMENT '修改人员',
PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=12 DEFAULT CHARSET=utf8 COMMENT='minio文件操作记录';
