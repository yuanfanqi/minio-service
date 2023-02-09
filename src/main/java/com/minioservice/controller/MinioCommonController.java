package com.minioservice.controller;

import com.minioservice.commonModel.ResultRes;
import com.minioservice.service.MinioBaseService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @Author: song
 * @Date: 2023/2/3 10:56
 * @Description:
 */
@RestController
@CrossOrigin
@RequestMapping(value = "/minio", produces = "application/json;charset=UTF-8")
public class MinioCommonController {

    private final static Logger logger = LoggerFactory.getLogger(MinioCommonController.class);

    @Autowired
    MinioBaseService minioBaseService;

    /**
     * @param: [inFile, prodCode, token]
     * @return: java.lang.String
     * @Author: song
     * @Date: 2023/2/3 11:49
     * @Description: 文件上传接口
     */
    @PostMapping("/minioUpload")
    @ResponseBody
    public ResultRes upload (@RequestParam("inFile") MultipartFile inFile,
                             @RequestParam("prodCode") String prodCode,
                             @RequestParam("token") String token,
                             @RequestParam(value = "tags",required = false) String tags,
                             @RequestParam("isReplace") boolean isReplace
    ) {
        logger.info("===================进入文件上传接口===================");
        //todo 参数验证
        //todo tags是否为中文
        try {
            return minioBaseService.doUpload(inFile, prodCode, token, tags, isReplace);
        } catch (Exception e) {
            logger.error(e.getMessage());
            return ResultRes.fail(e.getMessage());
        }
    }

    /**
     * @Description: 文件删除接口
     * @param: [fileName(文件名需带文件类型后缀), prodCode, token, isLogicDel]
     * @return: java.util.Map<java.lang.String,java.lang.String>
     * @Author: song
     * @Date: 2023/2/7 16:56
     */
    @PostMapping("/removeFile")
    @ResponseBody
    public Map<String, String> removeFile (
            @RequestParam("fileName") String fileName,
            @RequestParam("prodCode") String prodCode,
            @RequestParam("token") String token,
            @RequestParam("isLogicDel") boolean isLogicDel
    ) {
        logger.info("===================进入文件删除接口===================");
        Map<String, String> res = new HashMap<String, String>();
        //todo token验证
        try {
            res = minioBaseService.removeFile(fileName, prodCode, token, isLogicDel);
        } catch (Exception e) {
            res.put("STATUS", "E");
            res.put("MSG", "删除失败：" + e.getMessage());
        }
        logger.info("===================文件删除接口结束===================");
        return res;
    }

    /**
     * @Description: 文件列表查询
     * @param: [prodCode, token]
     * @return: java.util.Map<java.lang.String,java.lang.Object>
     * @Author: song
     * @Date: 2023/2/7 17:20
     */
    @PostMapping("/bucketFileList")
    @ResponseBody
    public Map<String, Object> bucketFileList(
            @RequestParam("prodCode") String prodCode,
            @RequestParam("token") String token
    ) {
        logger.info("===================进入文件列表查询接口===================");
        Map<String, Object> res = new HashMap<String, Object>();
        try {
            List<String> fileList = minioBaseService.getBucketFileList(prodCode, token);
            res.put("STATUS", "S");
            res.put("MSG", fileList);
        } catch (Exception e) {
            res.put("STATUS", "E");
            res.put("MSG", "查询失败：" + e.getMessage());
        }
        logger.info("===================文件列表查询接口结束===================");
        return res;
    }

    @PostMapping("/doDownload")
    @ResponseBody
    public Map<String, String> doDownload(
            @RequestParam("fileName") String fileName,
            @RequestParam("prodCode") String prodCode,
            @RequestParam("downloadPath") String downloadPath,
            @RequestParam("token") String token
    ) {
        logger.info("===================进入文件下载接口===================");
        Map<String, String> res = new HashMap<String, String>();
        try {
            res = minioBaseService.doDownload(fileName, prodCode, token, downloadPath);
        } catch (Exception e) {
            res.put("STATUS", "E");
            res.put("MSG", "下载失败：" + e.getMessage());
        }
        logger.info("===================文件下载接口结束===================");
        return res;
    }

    /**
     * @Description: 多文件上传
     * @param: [inFileList, prodCode, token, tags, isReplace]
     * @return: java.util.Map<java.lang.String,java.lang.String>
     * @Author: song
     * @Date: 2023/2/9 13:30
     */
    @PostMapping("/minioUploadFiles")
    @ResponseBody
    public ResultRes uploadFiles (@RequestParam("inFileList") MultipartFile[] inFileList,
                                       @RequestParam("prodCode") String prodCode,
                                       @RequestParam("token") String token,
                                       @RequestParam(value = "tags",required = false) String tags,
                                       @RequestParam("isReplace") boolean isReplace
    ) {
        logger.info("===================进入多文件上传接口===================");
        //todo 参数验证
        //todo tags是否为中文
        try {
            return minioBaseService.doUploadFiles(inFileList, prodCode, token, tags, isReplace);
        } catch (Exception e) {
            logger.error(e.getMessage());
            return ResultRes.fail(e.getMessage());
        }
    }
    //多文件下载
    //多文件删除
    //文件tags的修改
}
