package com.minioservice.dao;

import com.minioservice.entity.LogMinioOperate;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.data.domain.Pageable;

import java.util.List;

/**
 * minio文件操作记录(LogMinioOperate)表数据库访问层
 *
 * @author makejava
 * @since 2023-02-08 11:46:52
 */
@Mapper
public interface LogMinioOperateDao {

    /**
     * 通过ID查询单条数据
     *
     * @param id 主键
     * @return 实例对象
     */
    LogMinioOperate queryById(Long id);

    /**
     * 查询指定行数据
     *
     * @param logMinioOperate 查询条件
     * @param pageable        分页对象
     * @return 对象列表
     */
    List<LogMinioOperate> queryAllByLimit(LogMinioOperate logMinioOperate, @Param("pageable") Pageable pageable);

    /**
     * 统计总行数
     *
     * @param logMinioOperate 查询条件
     * @return 总行数
     */
    long count(LogMinioOperate logMinioOperate);

    /**
     * 新增数据
     *
     * @param logMinioOperate 实例对象
     * @return 影响行数
     */
    int insert(LogMinioOperate logMinioOperate);

    /**
     * 批量新增数据（MyBatis原生foreach方法）
     *
     * @param entities List<LogMinioOperate> 实例对象列表
     * @return 影响行数
     */
    int insertBatch(@Param("entities") List<LogMinioOperate> entities);

    /**
     * 批量新增或按主键更新数据（MyBatis原生foreach方法）
     *
     * @param entities List<LogMinioOperate> 实例对象列表
     * @return 影响行数
     * @throws org.springframework.jdbc.BadSqlGrammarException 入参是空List的时候会抛SQL语句错误的异常，请自行校验入参
     */
    int insertOrUpdateBatch(@Param("entities") List<LogMinioOperate> entities);

    /**
     * 修改数据
     *
     * @param logMinioOperate 实例对象
     * @return 影响行数
     */
    int update(LogMinioOperate logMinioOperate);

    /**
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 影响行数
     */
    int deleteById(Long id);

    /**
     * 查询单个文件是否存在：文件名or文件hash是否相同
     * @param fileName
     * @param fileHash
     * @param sourceService
     * @return LogMinioOperate
     */
    List<LogMinioOperate> isFileExist(@Param("fileName") String fileName,
                             @Param("fileHash") String fileHash,
                             @Param("sourceService") String sourceService);
}

