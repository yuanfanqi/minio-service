package com.minioservice.service;


import com.minioservice.entity.LogMinioOperate;
import org.apache.ibatis.annotations.Param;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import java.util.List;

/**
 * minio文件操作记录(LogMinioOperate)表服务接口
 *
 * @author makejava
 * @since 2023-02-08 14:30:41
 */
public interface LogMinioOperateService {

    /**
     * 通过ID查询单条数据
     *
     * @param id 主键
     * @return 实例对象
     */
    LogMinioOperate queryById(Long id);

    /**
     * 分页查询
     *
     * @param logMinioOperate 筛选条件
     * @param pageRequest     分页对象
     * @return 查询结果
     */
    Page<LogMinioOperate> queryByPage(LogMinioOperate logMinioOperate, PageRequest pageRequest);

    /**
     * 新增数据
     *
     * @param logMinioOperate 实例对象
     * @return 实例对象
     */
    LogMinioOperate insert(LogMinioOperate logMinioOperate);

    /**
     * 修改数据
     *
     * @param logMinioOperate 实例对象
     * @return 实例对象
     */
    LogMinioOperate update(LogMinioOperate logMinioOperate);

    /**
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 是否成功
     */
    boolean deleteById(Long id);

    /**
     * @Description: 查询单个文件是否存在：文件名or文件hash是否相同
     * @param: [fileName, fileHash, sourceService]
     * @return: LogMinioOperate
     * @Author: song
     * @Date: 2023/2/9 9:53
     */
    List<LogMinioOperate> isFileExist(String fileName, String fileHash, String sourceService);
}
